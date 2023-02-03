{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol
  ( PhRequest (..),
    PhResponse (..),
    packRequest,
    handleRequest,
  )
where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Container
import Control.Monad.Writer.Strict
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString.Lazy

-- REQUEST PROTOCOL:
-- The first 8 bytes are an unsigned integer, representing the total
-- length of the rest of the payload (excluding those 8 bytes). After
-- that, the serialised request follows.
-- RESPONSE PROTOCOL:
-- same as above, but with PhResponse.

-- | This data type represent a single request to the server.
data PhRequest = PhRequest
  { -- | commands to be sent to run in order
    reqCommands :: [T.Text],
    -- | extra files to be inserted into container (Path, contents)
    --   (we currently don't support creating dirs, move them manually later)
    --   WARNING: this could overwrite files!
    reqFiles :: [(T.Text, LBS.ByteString)]
  }
  deriving (Generic, Serialise, Show)

-- | This data type represent a response to a request.
data PhResponse = PhResponse
  { respStdout :: T.Text,
    respStderr :: T.Text
  }
  deriving (Generic, Serialise, Show)

-- | Pack a CCASerialisable into a single message to be sent over the wire.
packRequest :: PhRequest -> LBS.ByteString
packRequest req =
  let s = serialise req
   in encode (LBS.length s) `LBS.append` s

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
                      ( \s -> do
                          contDo CCPutStr s
                          contDo CCWaitShCmd
                          tell . DL.singleton
                            =<< (,) <$> contDo CCGetAll CCOut <*> contDo CCGetAll CCErr
                      )
                      $ reqCommands request
              )
          -- Create our response and send it.
          let respBody = serialise $ uncurry PhResponse contOut
          let resp = encode (LBS.length respBody) `LBS.append` respBody
          sendAll sock resp
          return ()
      return ()

--recvResponse :: Socket -> IO ()
