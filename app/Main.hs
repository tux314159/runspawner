module Main (main) where

import Network.Run.TCP (runTCPServer)
import Network.Runspawner.Handlers (handleRequest)
import Control.Monad.Except (runExceptT, catchError, lift)
import Control.Monad (void)

main :: IO ()
main = void $ runTCPServer Nothing "3456" $
  \s -> runExceptT $ handleRequest s `catchError` (lift . putStrLn)
