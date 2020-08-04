module Estructuras where

import Data.Text (Text) 
import Data.Int (Int32) 

data Producto = Producto { codigo :: Int32,
                           nombre :: Maybe Text,
                           proveedor :: Maybe Text,
                           precio :: Maybe Double,
                           stock :: Maybe Int32 }

{- El par (Int,Int) representa (código,cantidad) donde:
  *código: es el código del producto que se vende
  *cantidad: es la cantidad de ese producto que se esta vendiendo -}
type Venta = [(Int32, Int32)]
