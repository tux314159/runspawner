module Main (main) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Writer.Strict (tell)
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import Data.Int (Int64)
import qualified Data.Store as Store
import qualified Data.Text as T
import Network.Runspawner.Protocol
import Network.Run.TCP (runTCPServer)
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
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
