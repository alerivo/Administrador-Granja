module Estructuras where

import Data.Text (Text) 
import Data.Int (Int32) 

-- ~ Especifica un producto, los campos Maybe son así porque son opcionales.
-- ~ Los productos cuyo stock sea Nothing significa que no tiene importancia su stock,
-- ~ algo así como si tuviera stock infinito.
data Producto = Producto { codigo :: Int32,
                           nombre :: Text,
                           proveedor :: Maybe Text,
                           precio :: Double,
                           stock :: Maybe Int32 }
