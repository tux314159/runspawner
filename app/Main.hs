module Main (main) where

import Container
import System.IO

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\inpp outpp errpp -> do
      hPutStr inpp "echo hi\n"
      hPutStr inpp "sleep 1\n"
      _ <- putStr <$> hShow outpp
      --hPutStr inpp "exit\n"
      return ()
    )
