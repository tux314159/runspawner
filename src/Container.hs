{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Container
  ( ContainerBase (..),
    Container (..),
    ContCtx (..),
    CCAction (..),
    CCGetLn (..),
    CCGetChar (..),
    CCPutStr (..),
    CCCopy (..),
    CCCopyExt (..),
    withContainer,
  )
where

import Shelly (liftIO, shelly, withTmpDir, (</>), cp_r)
import System.IO
import System.Process

newtype ContainerBase = ContainerBase {contBasePath :: FilePath}

data Container = Container {contBase :: ContainerBase, contInst :: FilePath}

-- | Contains all info about a container needed for its functions.
data ContCtx = ContCtx
  { ccInPp :: Handle,
    ccOutPp :: Handle,
    ccErrPp :: Handle,
    ccRealPath :: FilePath
  }

-- | Typeclass representing all actions on a container.
class CCAction a b | a -> b where
  contCtxDo :: ContCtx -> a -> b

-- | Constructs a container context and runs within it a computation.
withContainer :: ContainerBase -> ((forall act. forall out. CCAction act out => act -> out) -> IO ()) -> IO ()
withContainer base computation =
  shelly $
    withTmpDir
      ( \tdir -> do
          let contPath = tdir ++ "/cont"
          liftIO $ callProcess "/bin/cp" ["-R", contBasePath base, contPath]
          (Just inpipe, Just outpipe, Just errpipe, ph) <-
            liftIO . createProcess $
              (proc "systemd-nspawn" ["--pipe", "-q", "-D", contPath])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          liftIO $ mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
          liftIO $ mapM_ (`hSetBuffering` NoBuffering) [inpipe, outpipe, errpipe]
          liftIO
            ( computation $
                contCtxDo $
                  ContCtx inpipe outpipe errpipe contPath
            )
          _ <- liftIO $ waitForProcess ph
          return ()
      )

-- Now, we can define some more nice container actions.
{- ORMULU_DISABLE -}
-- | Read one line from container stdout.
data CCGetLn = CCGetLn
instance CCAction CCGetLn (IO String) where
  contCtxDo cctx _ = hGetLine $ ccInPp cctx

-- | Read one char from container stdout.
data CCGetChar = CCGetChar
instance CCAction CCGetChar (IO Char) where
  contCtxDo cctx _ = hGetChar $ ccInPp cctx

-- | Write a string to container stdin.
data CCPutStr = CCPutStr
instance CCAction CCPutStr (String -> IO ()) where
  contCtxDo cctx _ = hPutStr $ ccOutPp cctx

-- | Like just running cp normally.
data CCCopy = CCCopy
instance CCAction CCCopy (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = hPutStr (ccInPp cctx) $ "cp " ++ src ++ " " ++ dest ++ "\n"

-- | Copy a file/dir recursively from the outside into the container.
data CCCopyExt = CCCopyExt
instance CCAction CCCopyExt (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = shelly $ cp_r src $ ccRealPath cctx </> dest

{- ORMULU_ENABLE -}
