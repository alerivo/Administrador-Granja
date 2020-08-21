module Main where

import BibliotecaBD
import Inicializador
import GUI.GUI

main :: IO ()
main = do conn <- conectarDB
          iniciar conn
          gui conn
          desconectarDB conn
