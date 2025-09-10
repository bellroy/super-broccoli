{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Api
  ( AddItemRequest (..),
    Cart (..),
    CartItem (..),
    CreateCartRequest (..),
    Product (..),
    ShoppingCartRoutes (..),
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Database.SQLite.Simple (Connection, Only (..), execute, executeMany, execute_, open, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (runEnv)
import Servant (Handler, err404, err500, throwError)
import Servant.Server.Generic (AsServerT, genericServeT)

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field

instance ToRow Product

newtype DBCart = DBCart
  {id :: Int}
  deriving (Generic, Show, FromRow, ToRow)

data DBCartItem = DBCartItem
  { id :: Int,
    dbCartId :: Int,
    productId :: Int,
    quantity :: Int
  }
  deriving (Generic, Show, ToRow)

instance FromRow DBCartItem where
  fromRow = do
    id <- field
    dbCartId <- field
    productId <- field
    quantity <- field
    pure DBCartItem {..}

type AppM = ReaderT Connection Handler

handlers :: ShoppingCartRoutes (AsServerT AppM)
handlers =
  ShoppingCartRoutes
    { getCart = getCartHandler,
      createCart = createCartHandler,
      addItem = addItemHandler,
      removeItem = removeItemHandler
    }

getCartHandler :: Int -> AppM Cart
getCartHandler cartId = do
  connection <- ask
  dbCarts :: [DBCart] <- liftIO . query connection "SELECT * FROM carts WHERE id = ?" $ Only cartId

  case dbCarts of
    [] -> throwError err404
    [dbCart] -> do
      items <- liftIO . query connection "SELECT * FROM cart_items WHERE cart_id = ?" $ Only cartId
      pure
        Cart
          { id = cartId,
            items = map (\DBCartItem {productId, quantity} -> CartItem {..}) items
          }
    _ -> throwError err500

createCartHandler :: CreateCartRequest -> AppM Cart
createCartHandler (CreateCartRequest items) = do
  connection <- ask
  allCarts <- liftIO $ query_ connection "SELECT * FROM carts"
  let nextId = if null allCarts then 1 else maximum (map (\(DBCart {id}) -> id) allCarts) + 1
      newCart = DBCart {id = nextId}
  liftIO $ execute connection "INSERT INTO carts (id) VALUES (?)" newCart
  pure Cart {id = nextId, items = []}

addItemHandler :: Int -> AddItemRequest -> AppM Cart
addItemHandler cartId (AddItemRequest prodId qty) = do
  connection <- ask
  dbCarts :: [DBCart] <- liftIO . query connection "SELECT * FROM carts WHERE id = ?" $ Only cartId

  case dbCarts of
    [] -> throwError err404
    [dbCart] -> do
      let newItem = DBCartItem {productId = prodId, quantity = qty}
      liftIO $ execute connection "INSERT INTO cart_items (cart_id, product_id, quantity) VALUES (?, ?, ?)" (cartId, prodId, qty)
      items <- liftIO . query connection "SELECT * FROM cart_items WHERE cart_id = ?" $ Only cartId
      pure
        Cart
          { id = cartId,
            items = map (\DBCartItem {productId, quantity} -> CartItem {..}) items
          }
    _ -> throwError err500

removeItemHandler :: Int -> Int -> AppM Cart
removeItemHandler cartId prodId = do
  connection <- ask
  dbCarts :: [DBCart] <- liftIO . query connection "SELECT * FROM carts WHERE id = ?" $ Only cartId

  case dbCarts of
    [] -> throwError err404
    [dbCart] -> do
      liftIO . execute connection "DELETE FROM cart_items WHERE cart_id = ?" $ Only cartId
      items <- liftIO . query connection "SELECT * FROM cart_items WHERE cart_id = ?" $ Only cartId
      pure
        Cart
          { id = cartId,
            items = map (\DBCartItem {productId, quantity} -> CartItem {..}) items
          }
    _ -> throwError err500

initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  execute_ conn "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT NOT NULL, price REAL NOT NULL)"
  execute_ conn "CREATE TABLE carts (id INTEGER PRIMARY KEY)"
  execute_ conn "CREATE TABLE cart_items (id INTEGER PRIMARY KEY, cart_id INTEGER REFERENCES carts(id), product_id INTEGER, quantity INTEGER)"
  let products :: [(Int, String, Double)]
      products = [(1, "Slim Sleeve", 119.00), (2, "Hide & Seek", 129.00)]
  executeMany
    conn
    "INSERT INTO products (id, name, price) VALUES (?, ?, ?)"
    products

main :: IO ()
main = do
  putStrLn "Starting shopping cart server..."
  conn <- open "test.db"
  initializeDatabase conn
  runEnv 8080 $ genericServeT (`runReaderT` conn) handlers
