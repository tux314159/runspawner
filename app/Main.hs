module Main (main) where

import Container

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\contDo -> do
      contDo CCPutStr "echo hi\n"
      putStr =<< contDo CCGetLn
      contDo CCPutStr "echo bye\n"
      putStr =<< contDo CCGetLn
      return ()
    )
