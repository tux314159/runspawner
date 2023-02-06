module Main (main) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Writer.Strict (liftIO, tell)
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import Data.Int (Int64)
import qualified Data.Store as Store
import qualified Data.Text as T
import Network.Run.TCP (runTCPServer)
import Network.Runspawner.Protocol
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Nspawn.Container

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
        Right (request :: Request) -> do
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
              t0 <- liftIO $ getTime Monotonic
              contDo CCPutStr s
              contDo CCWaitShCmd
              t1 <- liftIO $ getTime Monotonic
              tell . DL.singleton . LBS.fromStrict . Store.encode
                =<< ContCmdOut
                  <$> contDo CCGetAll CCOut
                  <*> contDo CCGetAll CCErr
                  <*> pure (toNanoSecs (t1 - t0))
            mkResponse contOut =
              let respBody = serialise (map (Store.decodeEx . LBS.toStrict) contOut :: Response)
               in encode (LBS.length respBody) `LBS.append` respBody
      return ()
