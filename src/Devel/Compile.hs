{-|
Module      : Devel.Build
Description : Attempts to compile the WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

compile compiles the app to give:
Either a list of source errors or an ide-backend session.
-}

{-# LANGUAGE PackageImports, OverloadedStrings, TemplateHaskell #-}

module Devel.Compile (compile) where

-- Almost everything is dependent on ide-backend.
import IdeSession

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

-- Used internally for showing errors.
import Data.Text (unpack)

-- Utility functions
import Data.Monoid ((<>))
import System.Directory (createDirectoryIfMissing)

-- Local imports
import Devel.Paths
import Devel.Types

-- web sockets
import qualified Devel.WebSocket as DW
import Control.Concurrent (forkIO)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
import Network.Socket (accept, withSocketsDo, close, Socket)
import Devel.ReverseProxy (createSocket, checkPort)
import Data.IORef
import Control.Concurrent (threadDelay)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (shamletFile)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as Char8
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)

compile :: FilePath -> SessionConfig -> IO (Either [SourceError'] IdeSession)
compile buildFile config = do

  session <- initSession
             defaultSessionInitParams
             config

  extensionList <- extractExtensions
  let dumpDir = ".dist/dump-hi"
  _ <- createDirectoryIfMissing True dumpDir

  -- Description of session updates.
  let targetList = (TargetsInclude [buildFile] :: Targets)
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-ddump-hi", "-ddump-to-file", ("-dumpdir "++dumpDir)] ++ ["-Wall"] ++ extensionList)

  refCon <- newIORef (Nothing :: Maybe WS.Connection)

  -- Actually update the session.
  updateSession session update (streamOutput refCon)
  
  let ioMaybRef = readIORef refCon
  maybeRef <- ioMaybRef
  case maybeRef of
    Nothing -> return ()
    Just conn -> WS.sendClose conn ("Connection closed" :: BS.ByteString)

  -- Custom error showing.
  errorList' <- getSourceErrors session

  let errorList = case filterErrors errorList' of
                    [] -> []
                    _  -> prettyPrintErrors errorList'

  --  We still want to see errors and warnings on the terminal.
  mapM_ putStrLn $ prettyPrintErrors errorList'

  return $ case errorList of
             [] -> Right session  
             _  -> Left  errorList

  where streamOutput :: IORef (Maybe WS.Connection) -> UpdateStatus -> IO ()
        streamOutput refCon status = do
          let refCon' = readIORef refCon
          maybeCon <- refCon'
          case maybeCon of
            Nothing -> do
              _ <- forkIO $ runS status refCon
              threadDelay 100000
              -- _ <- forkIO $ DW.runC
              _ <- forkIO $ buildProgress
              return ()
            Just conn -> WS.sendTextData conn $ BS.pack $ show status
          
          where runS :: UpdateStatus -> IORef (Maybe WS.Connection) -> IO ()
                runS status refCon = do
                  sock' <- createSocket 4000
                  (sock, _) <- accept sock'
                  pending <- WS.makePendingConnection sock WS.defaultConnectionOptions
                  conn <- WS.acceptRequest pending
                  WS.sendTextData conn $ BS.pack $ show status
                  writeIORef refCon (Just conn)


-- | Remove the warnings from [SourceError] if any.
-- Return an empty list if there are no errors and only warnings
-- Return non empty list if there are errors.
filterErrors :: [SourceError] -> [SourceError]
filterErrors [] = []
filterErrors (x:xs) = case errorKind x  of
             KindWarning -> filterErrors xs
             _ -> x : filterErrors xs

prettyPrintErrors :: [SourceError] -> [SourceError']
prettyPrintErrors [] = []
prettyPrintErrors (x: xs) = 
  case errorKind x  of
    KindWarning -> ("Warning: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x))) : prettyPrintErrors xs
    KindError   -> ("Error: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x)))  : prettyPrintErrors xs
    KindServerDied -> (show (errorKind x)) : prettyPrintErrors xs

-- | Parse the cabal file to extract the cabal extensions in use.
extractExtensions :: IO [String]
extractExtensions = do               
              cabalFilePath <- getCabalFile
              cabalFile <- readFile cabalFilePath

              let unsafePackageDescription = parsePackageDescription cabalFile

                  genericPackageDescription = case unsafePackageDescription of
                                            ParseOk _ a -> a
                                            _           -> error "failed package description."

                  packDescription = flattenPackageDescription genericPackageDescription

              rawExt <- return $ usedExtensions $ head $ allBuildInfo packDescription
              let parseExtension :: Extension -> String
                  parseExtension (EnableExtension extension) =  "-X" ++ (show extension)
                  parseExtension (DisableExtension extension) = "-XNo" ++ (show extension)
                  parseExtension (UnknownExtension extension) = "-X" ++ (show extension)

                  extensions = map parseExtension rawExt
              return extensions
application _ respond = do 
  respond $
    responseLBS 
      status200 
      [("Content-Type", "text/html")]           
      (Char8.fromStrict $ encodeUtf8 $ toStrict $ renderHtml $(shamletFile "build.hamlet"))

buildProgress :: IO ()
buildProgress = run 4001 application
