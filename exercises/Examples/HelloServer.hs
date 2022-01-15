module Examples.HelloServer where

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

port :: Int
port = 3421

main :: IO ()
main = run port application

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application :: Application
application request respond =
  respond (responseLBS status200 [] (BL.pack "Hello World!"))
