module Main (main) where

import Control.Monad (void)
import Control.Monad.Except (catchError, lift, runExceptT)
import qualified Data.Text as T

main :: IO ()
main = putStrLn "Hello"
