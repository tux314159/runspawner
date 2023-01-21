{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Container

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\contDo -> do
      contDo PutStr "echo hi\n"
      putStr =<< contDo GetLn
      contDo PutStr "echo bye\n"
      putStr =<< contDo GetLn
      return ()
    )
