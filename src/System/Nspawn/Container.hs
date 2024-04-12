{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module System.Nspawn.Container
  ( ContainerBase (..),
    Container (..),
    ContCtx (..),
    CCAction (..),
    CCGetAll (..),
    CCGetLine (..),
    CCGetChar (..),
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
import Data.DList (DList, toList)
import Data.List (intersperse)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Posix.Files
import Control.Monad ((<=<), when, void)

-- | Copy a directory recursively.
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dest = do
  createDirectory dest
  copyPermissions src dest
  files <- listDirectory src
  doesFileExist? copyFileWithMetadata `forAll` files
  doesNonSymlinkedDirectoryExist? copyDirRecursive `forAll` files
  where
    f ..^ g = pure . f <=< g
    f &&&^ g = \x -> do a <- f x; b <- g x; pure $ a && b
    (?) cond act f = cond (src </> f) >>= (`when` act (src </> f) (dest </> f))
    forAll = mapM_
    doesNonSymlinkedDirectoryExist = doesDirectoryExist &&&^ (not ..^ pathIsSymbolicLink)

-- | Create a temporary directory.
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
jobPipePath :: String
jobPipePath = "/var/lib/runspawner-job-pipe"

type CCmdOutW a = WriterT (DList T.Text) (ExceptT T.Text IO) a

-- | Constructs a container context and runs within it a computation.
withContainer ::
  (MonadIO m) =>
  ContainerBase ->
  ((forall act. forall out. CCAction act out => act -> out) -> CCmdOutW a) ->
  m (Either T.Text [T.Text])
withContainer base computation = do
  liftIO $ withSystemTempDirectory ""
    ( \contPath -> do
        -- Copy base container to temp container.
        removeDirectory contPath
        copyDirRecursive (contBasePath base) contPath

        -- Create the job pipe.
        createNamedPipe (contPath ++ jobPipePath) $ stdFileMode `unionFileModes` namedPipeMode
        -- Start the container.
        (Just inpipe, Just outpipe, Just errpipe, ph) <-
          createProcess $
            (proc "systemd-nspawn" ["-q", "--console=interactive", "-D", contPath, "/bin/sherver"])
              { std_in = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        _ <- hGetLine outpipe -- empty line emitted by sherver to signal ready
        jobCtlPipe <- openFile (contPath ++ jobPipePath) ReadMode
        -- Set modes.

        mapM_ (`hSetBinaryMode` True) [inpipe, outpipe, errpipe]
        mapM_ (`hSetBuffering` LineBuffering) [inpipe, outpipe, errpipe]

        -- Now we run the computation.
        compWriter' <-
          runExceptT $
            execWriterT $
              computation $
                contCtxDo $
                  ContCtx inpipe outpipe errpipe contPath jobCtlPipe

        _ <- waitForProcess ph

        hClose jobCtlPipe
        case compWriter' of
          Left err -> pure $ Left err
          Right compWriter -> pure . Right $ toList compWriter
    )

-- Now, we can define some more nice container actions.

-- | Selector for container stdout or stderr.
data CCOutStream = CCOut | CCErr

selectStream :: CCOutStream -> (ContCtx -> Handle)
selectStream CCOut = ccOutPp
selectStream CCErr = ccErrPp

-- | Read one char from container stdout.
data CCGetChar = CCGetChar

instance CCAction CCGetChar (CCOutStream -> CCmdOutW Char) where
  contCtxDo cctx _ stream =
    liftIO . hGetChar $ selectStream stream cctx

-- | Read one line from container stdout, omitting the trailing newline.
data CCGetLine = CCGetLine

instance CCAction CCGetLine (CCOutStream -> CCmdOutW T.Text) where
  contCtxDo cctx _ stream =
    T.pack <$> (liftIO . hGetLine $ selectStream stream cctx)

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
          then pure . reverse . intersperse "\n" $ "" : s
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
  contCtxDo cctx _ src dest = liftIO $ copyDirRecursive (T.unpack src) $ ccRealPath cctx ++ T.unpack dest
