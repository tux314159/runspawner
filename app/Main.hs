module Main (main) where

import Control.Monad (void)
import Control.Monad.Except (catchError, runExceptT)
import qualified Data.Text as T

main :: IO ()
main = putStrLn "Hello"
