{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Container
  ( ContainerBase (..),
    Container (..),
    ContCtx (..),
    CCAction (..),
    CCGetAll (..),
    CCGetLine (..),
    CCGetChar (..),
    CCPutStr (..),
    CCWaitShCmd (..),
    CCCopy (..),
    CCCopyExt (..),
    withContainer,
  )
where

import Shelly (cp_r, liftIO, shelly, withTmpDir)
import System.IO
import System.Process
import Control.Monad (void)

newtype ContainerBase = ContainerBase {contBasePath :: FilePath}

data Container = Container {contBase :: ContainerBase, contInst :: FilePath}

-- | Contains all info about a container needed for its functions.
data ContCtx = ContCtx
  { ccInPp :: Handle,
    ccOutPp :: Handle,
    ccErrPp :: Handle,
    ccRealPath :: FilePath,
    ccJobCtlPipe :: Handle
  }

-- | Typeclass representing all actions on a container.
class CCAction a b | a -> b where
  contCtxDo :: ContCtx -> a -> b

-- | Where our job-control pipe is
phpPipePath :: String
phpPipePath = "/var/lib/pheidippides-job-pipe"

-- | Constructs a container context and runs within it a computation.
withContainer :: ContainerBase -> ((forall act. forall out. CCAction act out => act -> out) -> IO ()) -> IO ()
withContainer base computation =
  shelly $
    withTmpDir
      ( \tdir -> do
          let contPath = tdir ++ "/cont"

          -- Copy base container to temp container.
          liftIO $ callProcess "/bin/cp" ["-R", contBasePath base, contPath]

          -- Start the container.
          (Just inpipe, Just outpipe, Just errpipe, ph) <-
            liftIO . createProcess $
              (proc "systemd-nspawn" ["-q", "--console=interactive", "-D", contPath, "/bin/shserver"])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          _ <- liftIO $ hGetLine outpipe  -- empty line emitted by shserver to signal ready
          jobCtlPipe <- liftIO $ openFile (contPath ++ phpPipePath) ReadMode

          -- Set modes.
          liftIO $ mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
          liftIO $ mapM_ (`hSetBuffering` NoBuffering) [inpipe, outpipe, errpipe]

          -- Now we run the computation.
          liftIO
            ( computation $
                contCtxDo $
                  ContCtx inpipe outpipe errpipe contPath jobCtlPipe
            )
          _ <- liftIO $ waitForProcess ph

          liftIO $ hClose jobCtlPipe
          return ()
      )

-- Now, we can define some more nice container actions.
{- ORMOLU_DISABLE -}

-- | Read one char from container stdout.
data CCGetChar = CCGetChar
instance CCAction CCGetChar (IO Char) where
  contCtxDo cctx _ = hGetChar $ ccOutPp cctx

-- | Read one line from container stdout.
data CCGetLine = CCGetLine
instance CCAction CCGetLine (IO String) where
  contCtxDo cctx _ = hGetLine $ ccOutPp cctx

-- | Read everything from container stdout.
data CCGetAll = CCGetAll
instance CCAction CCGetAll (IO String) where
  contCtxDo cctx _ = getAll (ccOutPp cctx) ""
    where
      getAll outPp s = do
        isready <- not <$> hReady outPp
        if isready
          then return $ reverse s
          else getAll outPp . (: s) =<< contCtxDo cctx CCGetChar

-- | Write a string to container stdin.
data CCPutStr = CCPutStr
instance CCAction CCPutStr (String -> IO ()) where
  contCtxDo cctx _ s = do
    hPutStr (ccInPp cctx) s 
    hFlush $ ccInPp cctx

-- | Wait for the current command to be done executing.
data CCWaitShCmd = CCWaitShCmd
instance CCAction CCWaitShCmd (IO ()) where
  contCtxDo cctx _ = void $ hGetChar (ccJobCtlPipe cctx)

-- | Like just running cp normally.
data CCCopy = CCCopy
instance CCAction CCCopy (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = contCtxDo cctx CCPutStr $ "cp " ++ src ++ " " ++ dest ++ "\n"

-- | Copy a file/dir recursively from the outside into the container.
data CCCopyExt = CCCopyExt
instance CCAction CCCopyExt (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = shelly $ cp_r src $ ccRealPath cctx ++ dest

{- ORMOLU_ENABLE -}
