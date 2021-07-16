{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Schema
import Servant
import Users (users)

type API = FactoringAPI

startApp :: IO ()
startApp = do
  print "hello"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server x = getXY :<|> postX
  where
    getXY Nothing = return x
    getXY (Just y) = return (x + y)

    postX = return (x - 1)
