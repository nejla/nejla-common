{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module ServantApi where

import           Data.Text                (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Server

import qualified RestExample

type Api = "api" :> "1.0.0" :> Raw
           :<|> "users" :> Get '[JSON] [Text]

modifyPath :: ([Text] -> [Text]) -> Middleware
modifyPath f app request = app (request{pathInfo = f $ pathInfo request})

handler :: Server Api
handler = Tagged (modifyPath ("1.0.0" :) RestExample.api)
          :<|> return ["user3", "user4"]

main :: IO ()
main = Warp.run 8000 $  serve (Proxy @Api) handler
