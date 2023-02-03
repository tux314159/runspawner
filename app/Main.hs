module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Exception
import Control.Monad (forever, void)
import Network.Socket
import Protocol

main :: IO ()
main = runTCPServer Nothing "3456" handleRequest

-- | Run a TCP server. Taken from network-run.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

-- | Run a TCP client. Taken from network-run.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve Stream (Just host) port False
  bracket (open addr) (`gracefulClose` 5000) client
  where
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
    resolve socketType mhost port passive =
      head <$> getAddrInfo (Just hints) mhost (Just port)
      where
        hints =
          defaultHints
            { addrSocketType = socketType,
              addrFlags = [AI_PASSIVE | passive]
            }
