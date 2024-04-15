{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.Runspawner.Api
  ( RunspAPI,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Network.HTTP.Media ((//), (/:))
import Servant

type RunspAPI = "runcommand" :> QueryParam "cmd" String :> Get '[JSON] String
