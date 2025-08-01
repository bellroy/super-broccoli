{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.API (Capture, Delete, Get, JSON, Post, ReqBody, (:-), (:>))

data Product = Product
  { id :: Int,
    name :: String,
    price :: Double
  }
  deriving (Generic, Show)

instance ToJSON Product

instance FromJSON Product

data CartItem = CartItem
  { productId :: Int,
    quantity :: Int
  }
  deriving (Generic, Show)

instance ToJSON CartItem

instance FromJSON CartItem

data Cart = Cart
  { id :: Int,
    items :: [CartItem]
  }
  deriving (Generic, Show)

instance ToJSON Cart

instance FromJSON Cart

newtype CreateCartRequest = CreateCartRequest
  { items :: [CartItem]
  }
  deriving (Generic, Show)

instance ToJSON CreateCartRequest

instance FromJSON CreateCartRequest

data AddItemRequest = AddItemRequest
  { productId :: Int,
    quantity :: Int
  }
  deriving (Generic, Show)

instance ToJSON AddItemRequest

instance FromJSON AddItemRequest

data ShoppingCartRoutes mode = ShoppingCartRoutes
  { getCart :: mode :- "cart" :> Capture "cartId" Int :> Get '[JSON] Cart,
    createCart :: mode :- "cart" :> ReqBody '[JSON] CreateCartRequest :> Post '[JSON] Cart,
    addItem :: mode :- "cart" :> Capture "cartId" Int :> "items" :> ReqBody '[JSON] AddItemRequest :> Post '[JSON] Cart,
    removeItem :: mode :- "cart" :> Capture "cartId" Int :> "items" :> Capture "productId" Int :> Delete '[JSON] Cart
  }
  deriving (Generic)
