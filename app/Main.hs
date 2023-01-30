module Main (main) where

import Container

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\contDo -> do
      contDo CCPutStr "echo hi\n"
      contDo CCWaitShCmd
      contDo CCPutStr "echo bye\n"
      contDo CCWaitShCmd
      contDo CCPutStr "ls\n"
      contDo CCWaitShCmd
      putStr =<< contDo CCGetAll

      --getLine >>= contDo CCPutStr
      --contDo CCWaitShCmd
      --putStr =<< contDo CCGetAll
      return ()
    )
