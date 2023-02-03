{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Container
import Control.Monad.Writer.Strict (liftIO)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  _ <-
    withContainer
      (ContainerBase "/home/isaac/containers/alpine")
      ( \contDo -> do
          contDo CCPutStr "echo hi\n"
          contDo CCWaitShCmd
          liftIO . T.putStr =<< contDo CCGetLine CCOut

          contDo CCPutStr "echo bye\n"
          contDo CCWaitShCmd
          liftIO . T.putStrLn =<< contDo CCGetLine CCOut

          contDo CCPutStr "ls\n"
          contDo CCWaitShCmd
          liftIO . T.putStr =<< contDo CCGetAll CCOut

          return ()
      )
  return ()
