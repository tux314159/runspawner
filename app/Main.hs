module Main (main) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Runspawner.Api

server :: Server RunspAPI
server = return "Hello"

runspAPI :: Proxy RunspAPI
runspAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve runspAPI server

main :: IO ()
main = run 8081 app
