module Estructuras where

import Data.Text (Text) 
import Data.Int (Int32) 

data Producto = Producto { codigo :: Int32,
                           nombre :: Text,
                           proveedor :: Maybe Text,
                           precio :: Double,
                           stock :: Maybe Int32 }
