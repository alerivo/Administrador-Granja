module Main where

import Biblioteca
import Inicializador
import Estructuras
import GUI.GUI
-- ~ import Data.Text (Text,pack)

main :: IO ()
main = do conn <- conectarDB
          iniciar conn
          -- ~ agregarProducto prod1 conn
          -- ~ agregarProducto prod2 conn
          -- ~ test conn
          gui conn
          desconectarDB conn

-- ~ prod1 = Producto 501 (Just $ pack "oreo rellena") (Just $ pack "tyna") (Just 12.5) (Just 50)
-- ~ prod2 = Producto 201 (Just $ pack "lamparita led violeta") Nothing (Just 5) Nothing
-- ~ prod3 = Producto 202 (Just $ pack "lamparita led roja") Nothing (Just 5) Nothing
-- ~ prod4 = Producto 203 (Just $ pack "lamparita led azul") Nothing (Just 5) Nothing
