module Main where

import Biblioteca
import Inicializador
-- ~ import Estructuras
import GUI
import Data.Convertible.Base

main :: IO ()
main = do conn <- conectarDB
          iniciar conn
          -- ~ agregarProducto prod1 conn
          gui conn
          desconectarDB conn

-- ~ prod1 = Producto 501 (Just "oreo rellena") (Just "tyna") (Just 12.5) (Just 0)
-- ~ prod2 = (201, Just "lamparita led violeta", Nothing, Just 5, Nothing)
-- ~ prod3 = (202, Just "lamparita led roja", Nothing, Just 5, Nothing)
-- ~ prod4 = (204, Just "lamparita led azul", Nothing, Just 5, Nothing)
-- ~ prod5 = (205, Just "lamparita led verdde", Nothing, Just 5, Nothing)
-- ~ prod6 = (206, Just "lamparita led amarilla", Nothing, Just 5, Nothing)
