-- | Simple runner functions, adapted from the network-run package.
module Network.Run.TCP
  ( runTCPClient,
    runTCPServer,
  )
where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket

-- | Running a TCP client with a connected socket.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve Stream (Just host) port False
  E.bracket (open addr) (`gracefulClose` 5000) client
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve Stream mhost port True
  E.bracket (open addr) close loop
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

resolve :: SocketType -> Maybe HostName -> ServiceName -> Bool -> IO AddrInfo
resolve socketType mhost port passive =
  head <$> getAddrInfo (Just hints) mhost (Just port)
  where
    hints =
      defaultHints
        { addrSocketType = socketType,
          addrFlags = [AI_PASSIVE | passive]
        }

openServerSocket :: AddrInfo -> IO Socket
openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  return sock
