module Main (main) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Container
import Control.Concurrent (forkFinally)
import Control.Exception
import Control.Monad (forever, void)
import Control.Monad.Writer.Strict (tell)
import Data.Binary (decodeOrFail, encode)
import qualified Data.Store as Store
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import Data.Int (Int64)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Protocol

main :: IO ()
main = runTCPServer Nothing "3456" handleRequest

-- | Handle a request. This is basically the whole server program.
handleRequest :: Socket -> IO ()
handleRequest sock = do
  msg <- decodeOrFail <$> recv sock 8
  case msg of
    Left (_, _, err) -> putStrLn err
    Right (_, _, reqLen :: Int64) -> do
      rawRequest <- recv sock reqLen
      case deserialiseOrFail rawRequest of
        Left err -> print err
        Right (request :: PhRequest) -> do
          -- Execute the request inside a container.
          contOut <-
            withContainer
              (ContainerBase "/home/isaac/containers/alpine")
              ( \contDo ->
                  do
                    -- Create files.
                    mapM_
                      (uncurry $ contDo CCInsertFile)
                      $ reqFiles request
                    -- Run each command in sequence.
                    mapM
                      (runCmdAndWait contDo)
                      $ reqCommands request
              )
          -- Create our response and send it.
          sendAll sock $ mkResponse contOut
          return ()
          where
            runCmdAndWait :: (forall act out. CCAction act out => act -> out) -> T.Text -> CCmdOutW ()
            runCmdAndWait contDo s = do
              contDo CCPutStr s
              contDo CCWaitShCmd
              tell . DL.singleton . LBS.fromStrict . Store.encode
                =<< (,) <$> contDo CCGetAll CCOut <*> contDo CCGetAll CCErr
            mkResponse contOut =
              let respBody = serialise (map (Store.decodeEx . LBS.toStrict) contOut :: [PhResponse])
               in encode (LBS.length respBody) `LBS.append` respBody
      return ()

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
