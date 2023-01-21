{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Container
  ( ContainerBase (..),
    Container (..),
    ContCtx (..),
    CCAction (..),
    GetLn (..),
    GetChar (..),
    PutStr (..),
    withContainer,
  )
where

import Shelly (liftIO, shelly, withTmpDir)
import System.IO
import System.Process

newtype ContainerBase = ContainerBase {contBasePath :: FilePath}
data Container = Container {contBase :: ContainerBase, contInst :: FilePath}
newtype ContCtx = ContCtx (Handle, Handle, Handle)

data GetLn = GetLn
data GetChar = GetChar
data PutStr = PutStr

class CCAction a b | a -> b where
  contCtxDo :: ContCtx -> a -> b
instance CCAction GetLn (IO String) where
  contCtxDo (ContCtx (_, outpp, _)) _ = hGetLine outpp
instance CCAction GetChar (IO Char) where
  contCtxDo (ContCtx (_, outpp, _)) _ = hGetChar outpp
instance CCAction PutStr (String -> IO ()) where
  contCtxDo (ContCtx (inpp, _, _)) _ = hPutStr inpp

withContainer :: ContainerBase -> ((forall act. forall out. CCAction act out => act -> out) -> IO ()) -> IO ()
withContainer base computation =
  shelly $
    withTmpDir
      ( \tdir -> do
          liftIO $ callProcess "/bin/cp" ["-R", contBasePath base, tdir ++ "/cont"]
          (Just inpipe, Just outpipe, Just errpipe, ph) <-
            liftIO . createProcess $
              (proc "systemd-nspawn" ["--pipe", "-q", "-D", tdir ++ "/cont"])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          liftIO $ mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
          liftIO $ mapM_ (`hSetBuffering` NoBuffering) [inpipe, outpipe, errpipe]
          liftIO (computation $ contCtxDo $ ContCtx (inpipe, outpipe, errpipe))
          _ <- liftIO $ waitForProcess ph
          return ()
      )


