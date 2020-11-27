module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)

import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Web.Utils
import Web.Response
import Web.Router
import Routes

import Settings (host, port)

main :: IO ()
main = do
  let hostStr = case host of
                  Just smt -> smt
                  Nothing  -> "0.0.0.0"

  putStrLn $ "Server launched on " ++ hostStr ++ ":" ++ port
  runTCPServer host port talk
  where
    talk s = do
      msg <- recv s 1024
      putStrLn "Got request"
      unless (S.null msg) $ do
        let (request, _, _) = parseHttp $ decodeUtf8 msg
        response <- resolve routesTable $ request
        sendAll s $ encodeUtf8 (formResponse response)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
