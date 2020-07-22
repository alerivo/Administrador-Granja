module Estructuras where

data Producto = Producto { codigo :: Int,
                           nombre :: Maybe String,
                           proveedor :: Maybe String,
                           precio :: Maybe Double,
                           stock :: Maybe Int }

{- El par (Int,Int) representa (código,cantidad) donde:
  *código: es el código del producto que se vende
  *cantidad: es la cantidad de ese producto que se esta vendiendo -}
type Venta = [(Int, Int)]
