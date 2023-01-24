module Main (main) where

import Container
import Control.Concurrent

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\contDo -> do
      contDo CCPutStr "echo hi\n"
      putStr =<< contDo CCGetAll
      contDo CCPutStr "echo bye\n"
      putStr =<< contDo CCGetAll

      getLine >>= contDo CCShellCmd
      contDo CCWaitShCmd
      putStr =<< contDo CCGetAll
      return ()
    )
