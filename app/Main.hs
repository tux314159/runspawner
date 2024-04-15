module Main (main) where

import qualified Data.Text as T
import Network.Runspawner.Api
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Nspawn.Container
import Control.Monad.Writer.Strict (tell)

server :: Server RunspAPI
server = handleRunCmd
  where
    containerRunCmd :: String -> (forall act out. CCAction act out => act -> out) -> CCmdOutW ()
    containerRunCmd cmd contDo = do
      contDo CCPutStrLn $ T.pack cmd
      contDo CCWaitShCmd
      tell . pure =<< contDo CCGetAll CCOut

    handleRunCmd :: Maybe String -> Handler String
    handleRunCmd mcmd = do
      case mcmd of
        Just cmd ->
          T.unpack . either id mconcat
            <$> withContainer (ContainerBase "/home/isaac/containers/alpine") (containerRunCmd cmd)
        Nothing -> pure ""

runspAPI :: Proxy RunspAPI
runspAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve runspAPI server

main :: IO ()
main = run 8081 app
