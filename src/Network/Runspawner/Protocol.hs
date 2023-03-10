{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Runspawner.Protocol
  ( ContCmdOut (..),
    Request (..),
    Response,
    packRequest,
  )
where

import Codec.Serialise (Serialise, serialise)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Store (Store)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- REQUEST PROTOCOL:
-- The first 8 bytes are an unsigned integer, representing the total
-- length of the rest of the payload (excluding those 8 bytes). After
-- that, the serialised request follows.
-- RESPONSE PROTOCOL:
-- same as above, but with Response.

-- | This data type represent a single request to the server.
data Request = Request
  { -- | commands to be sent to run in order
    reqCommands :: [T.Text],
    -- | extra files to be inserted into container (Path, contents)
    --   (we currently don't support creating dirs, move them manually later)
    --   WARNING: this could overwrite files!
    reqFiles :: [(T.Text, LBS.ByteString)]
  }
  deriving (Generic, Serialise, Show)

-- | Data type representing the output, etc. of a single command.
data ContCmdOut = ContCmdOut
  { -- | Everything that was in standard output.
    ccoStdout :: T.Text,
    -- | Everything that was in standard error.
    ccoStderr :: T.Text,
    -- | Time it took to run each command, in nanoseconds(!)
    ccoTiming :: Integer
  }
  deriving (Generic, Store, Serialise, Show)

-- | This type represent a response to a request.
type Response = [ContCmdOut]

-- | Pack a CCASerialisable into a single message to be sent over the wire.
packRequest :: Request -> LBS.ByteString
packRequest req =
  let s = serialise req
   in encode (LBS.length s) <> s
