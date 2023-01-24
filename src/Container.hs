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
    CCGetLn (..),
    CCGetChar (..),
    CCPutStr (..),
    CCShellCmd (..),
    CCWaitShCmd (..),
    CCCopy (..),
    CCCopyExt (..),
    withContainer,
  )
where

import Debug.Trace
import Shelly (cp_r, liftIO, shelly, withTmpDir, (</>))
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
    ccRealPath :: FilePath
  }

-- | Typeclass representing all actions on a container.
class CCAction a b | a -> b where
  contCtxDo :: ContCtx -> a -> b

-- | Where our job-control pipe is
phpPipePath :: String
phpPipePath = "/var/run/pheidippides-pipe\n"

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
              (proc "systemd-nspawn" ["--pipe", "-q", "-D", contPath])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          -- Set modes.
          liftIO $ mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
          liftIO $ mapM_ (`hSetBuffering` NoBuffering) [inpipe, outpipe, errpipe]
          -- Create fifo in the new container, for communication.
          -- This is probably far from the best way to do this but... whatever :D
          liftIO $
            contCtxDo
              (ContCtx inpipe outpipe errpipe contPath)
              CCPutStr
              $ "mkfifo " ++ phpPipePath ++ "\n"
          --liftIO $ withFile phpPipePath ReadMode $ flip hSetBuffering NoBuffering
          -- Now we run the computation.
          liftIO
            ( computation $
                contCtxDo $
                  ContCtx inpipe outpipe errpipe contPath
            )
          _ <- liftIO $ waitForProcess ph
          return ()
      )

-- Now, we can define some more nice container actions.
{- ORMOLU_DISABLE -}

-- | Read one line from container stdout.
data CCGetLn = CCGetLn
instance CCAction CCGetLn (IO String) where
  contCtxDo cctx _ = hGetLine $ ccOutPp cctx

data CCGetAll = CCGetAll
instance CCAction CCGetAll (IO String) where
  contCtxDo cctx _ = getAll (ccOutPp cctx) ""
    where
      getAll outPp s = do
        isready <- not <$> hReady outPp
        if isready
          then return $ reverse s
          else getAll outPp . (: s) =<< contCtxDo cctx CCGetChar

-- | Read one char from container stdout.
data CCGetChar = CCGetChar
instance CCAction CCGetChar (IO Char) where
  contCtxDo cctx _ = hGetChar $ ccOutPp cctx

-- | Write a string to container stdin.
data CCPutStr = CCPutStr
instance CCAction CCPutStr (String -> IO ()) where
  contCtxDo cctx _ = hPutStr $ ccInPp cctx

-- | Run a command in the container shell as unpriviledged user (container must
--   have it in the foreground). In particular cmd must be escaped; it will be placed
--   inside double-quotes. Again this is quite hacky but oh well.
data CCShellCmd = CCShellCmd
instance CCAction CCShellCmd (String -> IO ()) where
  contCtxDo cctx _ cmd = do
    let !a=traceId $ "su user -c \"" ++ cmd ++ "\";" ++ "echo >" ++ phpPipePath ++ "\n"
    --contCtxDo cctx CCPutStr $
      --"su user -c \"" ++ cmd ++ "\";" ++ "echo -e '\\n' >" ++ phpPipePath ++ "\n"
    hPutStr (ccInPp cctx) $ "su user -c \"" ++ cmd ++ "\";" ++ "echo -e '\\n' >" ++ phpPipePath ++ "\n"
    let !a=traceId $ "su user -c \"" ++ cmd ++ "\";" ++ "echo -e '\\n' >" ++ phpPipePath ++ "\n"
    return ()

data CCWaitShCmd = CCWaitShCmd
instance CCAction CCWaitShCmd (IO ()) where
  contCtxDo cctx _ = void $ withFile (ccRealPath cctx </> phpPipePath) ReadMode hGetChar

-- | Like just running cp normally.
data CCCopy = CCCopy
instance CCAction CCCopy (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = contCtxDo cctx CCPutStr $ "cp " ++ src ++ " " ++ dest ++ "\n"

-- | Copy a file/dir recursively from the outside into the container.
data CCCopyExt = CCCopyExt
instance CCAction CCCopyExt (FilePath -> FilePath -> IO ()) where
  contCtxDo cctx _ src dest = shelly $ cp_r src $ ccRealPath cctx </> dest

{- ORMOLU_ENABLE -}
