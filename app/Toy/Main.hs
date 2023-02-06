{-# LANGUAGE OverloadedStrings #-}

module Toy.Main (main) where

import Control.Monad.Writer.Strict (liftIO)
import qualified Data.Text.IO as T
import System.Nspawn.Container

main :: IO ()
main = do
  _ <-
    withContainer
      (ContainerBase "/home/isaac/containers/alpine")
      ( \contDo -> do
          contDo CCPutStrLn "echo hi"
          contDo CCWaitShCmd
          liftIO . T.putStrLn =<< contDo CCGetLine CCOut

          contDo CCPutStrLn "echo bye"
          contDo CCWaitShCmd
          liftIO . T.putStrLn =<< contDo CCGetLine CCOut

          contDo CCPutStrLn "ls"
          contDo CCWaitShCmd
          liftIO . T.putStr =<< contDo CCGetAll CCOut

          return ()
      )
  return ()
