{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
  ( AddItemRequest (..),
    Cart (..),
    CartItem (..),
    CreateCartRequest (..),
    Product (..),
    ShoppingCartRoutes (..),
  )
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Database.SQLite.Simple (Connection, executeMany, execute_, open)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Network.Wai.Handler.Warp (runEnv)
import Servant (Handler, err404, throwError)
import Servant.Server.Generic (AsServerT, genericServeT)

newtype ServerState = ServerState
  { carts :: [(Int, Cart)]
  }
  deriving (Show)

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field

instance ToRow Product

type AppM = ReaderT Connection (StateT ServerState Handler)

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
  mCart <- gets (lookup cartId . carts)
  case mCart of
    Just cart -> pure cart
    Nothing -> throwError err404

createCartHandler :: CreateCartRequest -> AppM Cart
createCartHandler (CreateCartRequest items) = do
  allCarts <- gets carts
  let nextId = if null allCarts then 1 else maximum (map fst allCarts) + 1
      newCart = Cart {id = nextId, items = items}
  modify $ \state@ServerState {carts} ->
    state {carts = (nextId, newCart) : carts}
  pure newCart

addItemHandler :: Int -> AddItemRequest -> AppM Cart
addItemHandler cartId (AddItemRequest prodId qty) = do
  mCart <- gets (lookup cartId . carts)
  case mCart of
    Nothing -> throwError err404
    Just cart@(Cart {items = cartItems}) -> do
      let newItem = CartItem {productId = prodId, quantity = qty}
          updatedCart = (cart :: Cart) {items = newItem : cartItems}
      modify $ \state@ServerState {carts} ->
        state
          { carts =
              map
                ( \(cId, c) ->
                    if cId == cartId
                      then
                        (cId, updatedCart)
                      else (cId, c)
                )
                carts
          }
      pure updatedCart

removeItemHandler :: Int -> Int -> AppM Cart
removeItemHandler cartId prodId = do
  mCart <- gets (lookup cartId . carts)
  case mCart of
    Nothing -> throwError err404
    Just cart@(Cart {items = cartItems}) -> do
      let updatedItems = filter (\CartItem {productId} -> productId /= prodId) cartItems
          updatedCart = (cart :: Cart) {items = updatedItems}
      modify $ \state@ServerState {carts} ->
        state
          { carts =
              map (\(cId, c) -> if cId == cartId then (cId, updatedCart) else (cId, c)) carts
          }
      pure updatedCart

initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  execute_ conn "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT NOT NULL, price REAL NOT NULL)"
  let products :: [(Int, String, Double)]
      products = [(1, "Slim Sleeve", 119.00), (2, "Hide & Seek", 129.00)]
  executeMany
    conn
    "INSERT INTO products (id, name, price) VALUES (?, ?, ?)"
    products

main :: IO ()
main = do
  putStrLn "Starting shopping cart server..."
  conn <- open ":memory:"
  initializeDatabase conn
  let initialState = ServerState {carts = []}
  runEnv 8080 $ genericServeT (\handler -> runReaderT handler conn `evalStateT` initialState) handlers
