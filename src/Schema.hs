{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Schema
  ( FactoringAPI,
    User (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import GHC.TypeLits
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId :: Int,
    userFirstName :: String,
    userLastName :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type FactoringAPI =
  "x" :> Capture "x" Int
    :> ( QueryParam "y" Int :> Get '[JSON] Int
           :<|> Post '[JSON] Int
       )
