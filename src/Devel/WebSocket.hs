{-# LANGUAGE OverloadedStrings #-}
module Devel.WebSocket where

import qualified Data.ByteString.Char8 as BS

import Control.Concurrent (forkIO)
import qualified Network.WebSockets as WS
import Network.Socket      (withSocketsDo, accept)
import Control.Monad       (forever)
import Control.Concurrent (threadDelay)

-- Client
-- Use forever to run client until connection is closed by the server
client :: WS.ClientApp ()
client conn = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  print msg

runC :: IO ()
runC = withSocketsDo $ WS.runClient "127.0.0.1" 4000 "/" client

-- Server
runS :: IO ()
runS = withSocketsDo $ do
  sock' <- WS.makeListenSocket "127.0.0.1" 4000
  (sock, _) <- accept sock'
  pending <- WS.makePendingConnection sock WS.defaultConnectionOptions
  conn <- WS.acceptRequest pending
  _    <- WS.sendTextData conn ("Test data" :: BS.ByteString)
  WS.sendClose conn ("Connection closed" :: BS.ByteString)
