{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Container
  ( ContainerBase (..),
    Container (..),
    withContainer,
  )
where

import qualified Data.Text as T
import Shelly (liftIO, shelly, withTmpDir)
import System.IO
import System.Process
import Data.Maybe (fromJust)

newtype ContainerBase = ContainerBase {contBasePath :: FilePath}

data Container = Container {contBase :: ContainerBase, contInst :: FilePath}

withContainer :: ContainerBase -> (Handle -> Handle -> Handle -> IO ()) -> IO ()
withContainer base computation =
  shelly $
    withTmpDir
      ( \tdir -> do
          liftIO $ callProcess "/bin/cp" ["-R", contBasePath base, tdir ++ "/cont"]
          (Just inpipe, Just outpipe, Just errpipe, ph) <-
            liftIO . createProcess_ "" $
              (proc "systemd-nspawn" ["--pipe", "-q", "-D", tdir ++ "/cont"])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          liftIO $ computation inpipe outpipe errpipe
          _ <- liftIO $ waitForProcess ph
          return ()
      )
