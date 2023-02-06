{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Nspawn.Container
  ( ContainerBase (..),
    Container (..),
    ContCtx (..),
    CCAction (..),
    CCGetAll (..),
    CCGetLine (..),
    CCGetChar (..),
    CCInsertFile (..),
    CCPutStr (..),
    CCPutStrLn (..),
    CCWaitShCmd (..),
    CCCopyExt (..),
    CCOutStream (..),
    CCmdOutW,
    withContainer,
  )
where

import Control.Monad.Except
import Control.Monad.Writer.Strict
import qualified Data.ByteString.Lazy as LBS
import Data.DList
import Data.List (intersperse)
import qualified Data.Text as T
import Shelly (cp_r, shelly, withTmpDir)
import System.IO
import System.Process

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

type CCmdOutW a = WriterT (DList LBS.ByteString) (ExceptT T.Text IO) a

-- | Constructs a container context and runs within it a computation.
withContainer ::
  (MonadIO m) =>
  ContainerBase ->
  ((forall act. forall out. CCAction act out => act -> out) -> CCmdOutW a) ->
  m (Either T.Text [LBS.ByteString])
withContainer base computation = do
  shelly $
    withTmpDir
      ( \contPath -> do
          -- Copy base container to temp container.
          liftIO $ callProcess "/bin/rmdir" [contPath]
          liftIO $ callProcess "/bin/cp" ["-R", contBasePath base, contPath]

          -- Start the container.
          (Just inpipe, Just outpipe, Just errpipe, ph) <-
            liftIO . createProcess $
              (proc "systemd-nspawn" ["-q", "--console=interactive", "-D", contPath, "/bin/sherver"])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          _ <- liftIO $ hGetLine outpipe -- empty line emitted by shserver to signal ready
          jobCtlPipe <- liftIO $ openFile (contPath ++ phpPipePath) ReadMode

          -- Set modes.
          liftIO $ mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
          liftIO $ mapM_ (`hSetBuffering` LineBuffering) [inpipe, outpipe, errpipe]

          -- Now we run the computation.
          compWriter' <-
            liftIO $
              runExceptT $
                execWriterT $
                  computation $
                    contCtxDo $
                      ContCtx inpipe outpipe errpipe contPath jobCtlPipe
                  
          _ <- liftIO $ waitForProcess ph

          liftIO $ hClose jobCtlPipe
          case compWriter' of
            Left err -> return $ Left err
            Right compWriter -> return . Right $ toList compWriter
      )

-- Now, we can define some more nice container actions.

-- | Selector for container stdout or stderr.
data CCOutStream = CCOut | CCErr

-- | Read one char from container stdout.
data CCGetChar = CCGetChar

instance CCAction CCGetChar (CCOutStream -> CCmdOutW Char) where
  contCtxDo cctx _ stream =
    liftIO $
      hGetChar $
        ( case stream of
            CCOut -> ccOutPp
            CCErr -> ccErrPp
        )
          cctx

-- | Read one line from container stdout, omitting the trailing newline.
data CCGetLine = CCGetLine

instance CCAction CCGetLine (CCOutStream -> CCmdOutW T.Text) where
  contCtxDo cctx _ stream =
    T.pack
      <$> liftIO
        ( hGetLine $
            ( case stream of
                CCOut -> ccOutPp
                CCErr -> ccErrPp
            )
              cctx
        )

-- | Read everything from container stdout.
data CCGetAll = CCGetAll

instance CCAction CCGetAll (CCOutStream -> CCmdOutW T.Text) where
  contCtxDo cctx _ stream =
    let ppe = case stream of
          CCOut -> ccOutPp
          CCErr -> ccErrPp
     in T.concat <$> getAll' (ppe cctx) []
    where
      getAll' ppe s = do
        isready <- not <$> liftIO (hReady ppe)
        if isready
          then return . reverse . intersperse "\n" $ "" : s
          else getAll' ppe . (: s) =<< contCtxDo cctx CCGetLine stream

-- | Write a string to container stdin.
data CCPutStr = CCPutStr

instance CCAction CCPutStr (T.Text -> CCmdOutW ()) where
  contCtxDo cctx _ s = do
    liftIO $ hPutStr (ccInPp cctx) $ T.unpack s
    liftIO $ hFlush $ ccInPp cctx

-- | Write a string + newline to container stdin.
data CCPutStrLn = CCPutStrLn

instance CCAction CCPutStrLn (T.Text -> CCmdOutW ()) where
  contCtxDo cctx _ s = contCtxDo cctx CCPutStr $ s <> "\n"

-- | Wait for the current command to be done executing.
data CCWaitShCmd = CCWaitShCmd

instance CCAction CCWaitShCmd (CCmdOutW ()) where
  contCtxDo cctx _ = void $ liftIO $ hGetChar (ccJobCtlPipe cctx)

-- | Copy a file/dir recursively from the outside into the container.
data CCCopyExt = CCCopyExt

instance CCAction CCCopyExt (T.Text -> T.Text -> CCmdOutW ()) where
  contCtxDo cctx _ src dest = shelly $ cp_r (T.unpack src) $ ccRealPath cctx ++ T.unpack dest

-- | Insert a file into the container.
data CCInsertFile = CCInsertFile

instance CCAction CCInsertFile (T.Text -> LBS.ByteString -> CCmdOutW ()) where
  contCtxDo cctx _ path = liftIO . LBS.writeFile (ccRealPath cctx ++ T.unpack path)
