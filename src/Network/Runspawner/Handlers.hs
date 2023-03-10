module Network.Runspawner.Handlers
  ( handleRequest,
    getResponse,
  )
where

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer.Strict (liftIO, tell)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import qualified Data.Store as Store
import qualified Data.Text as T
import Network.Runspawner.Protocol
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Nspawn.Container

-- | Get a response over the wire.
getResponse :: (MonadIO m, MonadError T.Text m) => Socket -> m Response
getResponse sock = do
  respLen' <- B.decodeOrFail <$> liftIO (recv sock 8)
  case respLen' of
    Left (_, _, err) -> throwError $ T.pack err
    Right (_, _, respLen) -> do
      rawResponse <- liftIO $ recv sock respLen
      case deserialiseOrFail rawResponse of
        Left err -> throwError . T.pack $ show err
        Right (response :: Response) -> return response

-- | Handle a request. This is basically the whole server program.
handleRequest :: (MonadIO m, MonadError T.Text m) => Socket -> m ()
handleRequest sock = do
  msg <- B.decodeOrFail <$> liftIO (recv sock 8)
  case msg of
    Left (_, _, err) -> throwError $ T.pack err
    Right (_, _, reqLen) -> do
      rawRequest <- liftIO $ recv sock reqLen
      case deserialiseOrFail rawRequest of
        Left err -> throwError . T.pack $ show err
        Right (request :: Request) -> do
          -- Execute the request inside a container.
          contOut' <-
            liftIO $
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
          case contOut' of
            Left err -> throwError err
            Right contOut -> liftIO $ sendAll sock $ mkResponse contOut
          return ()
          where
            runCmdAndWait :: (forall act out. CCAction act out => act -> out) -> T.Text -> CCmdOutW ()
            runCmdAndWait contDo s = do
              t0 <- liftIO $ getTime Monotonic
              contDo CCPutStrLn s
              contDo CCWaitShCmd
              t1 <- liftIO $ getTime Monotonic
              tell . DL.singleton . LBS.fromStrict . Store.encode
                =<< ContCmdOut
                  <$> contDo CCGetAll CCOut
                  <*> contDo CCGetAll CCErr
                  <*> pure (toNanoSecs $ t1 - t0)
            mkResponse contOut =
              let respBody = serialise (map (Store.decodeEx . LBS.toStrict) contOut :: Response)
               in B.encode (LBS.length respBody) <> respBody
      return ()
