module Main (main) where

import Container
import System.IO

main :: IO ()
main =
  withContainer
    (ContainerBase "/home/isaac/containers/alpine")
    (\inpp outpp errpp -> do
      hPutStr inpp "echo hi\n"
      putStrLn "a"
      putStr =<< hGetContents outpp
      putStr =<< hGetContents outpp
      putStrLn "b"
      --hPutStr inpp "echo hi\n"
      --putStr =<< hGetContents outpp
      return ()
    )
