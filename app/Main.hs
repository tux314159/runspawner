module Main (main) where

import Control.Monad (void)
import Control.Monad.Except (catchError, lift, runExceptT)
import qualified Data.Text as T
import Network.Run.TCP (runTCPServer)
import Network.Runspawner.Handlers (handleRequest)

main :: IO ()
main = void $
  runTCPServer Nothing "3456" $
    \s -> runExceptT $ handleRequest s `catchError` (lift . putStrLn . T.unpack)
