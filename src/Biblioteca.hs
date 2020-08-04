module Biblioteca where

import Estructuras
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Int (Int32)

conectarDB :: IO Connection 
conectarDB = connectSqlite3 "Base de datos/granja.db"

desconectarDB :: Connection -> IO ()
desconectarDB conn = disconnect conn

-- ~ El valor booleano representa si el producto fue agregado o no
agregarProducto :: Producto -> Connection -> IO Bool
agregarProducto prod conn =
  let codigo_ = codigo prod
      Just nombre_ = nombre prod
      proveedor_ = proveedor prod
      Just precio_ = precio prod
      stock_ = stock prod
      
      codigo_sql = toSql codigo_
      nombre_sql = toSql nombre_
      proveedor_sql = if isJust proveedor_ then toSql (fromJust proveedor_) else SqlNull
      precio_sql = toSql precio_
      stock_sql = if isJust stock_ then toSql (fromJust stock_) else SqlNull
      
      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argsSelect = [codigo_sql]
      
      queryInsert = "INSERT INTO productos VALUES (?,?,?,?,?)"
      argsInsert = [codigo_sql,nombre_sql,proveedor_sql,precio_sql,stock_sql]
  in
  do  [[resultado]] <- quickQuery' conn querySelect argsSelect
      if ((fromSql resultado) :: Int) == 0
        then do filasModificadas <- run conn queryInsert argsInsert
                commit conn
                return $ toEnum (fromInteger filasModificadas)
        else return False

-- ~ El valor booleano representa si el producto fue eliminado o no
eliminarProducto :: Producto -> Connection -> IO Bool
eliminarProducto prod conn =
  let codigo_sql = toSql $ codigo prod
      
      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      
      queryDelete = "DELETE FROM productos WHERE codigo = ?"

      arg = [codigo_sql]
  in
  do  [[resultado]] <- quickQuery' conn querySelect arg
      if ((fromSql resultado) :: Int) == 1
        then do filasModificadas <- run conn queryDelete arg
                commit conn
                return $ toEnum (fromInteger filasModificadas)
        else return False

-- ~ El valor booleano representa si el nombre fue actualizado o no
actualizarNombre :: Connection -> Producto -> Text -> IO Bool
actualizarNombre conn prod nombre = 
  let codigo_sql = toSql $ codigo prod
      nombre_sql = toSql nombre

      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect = [codigo_sql]

      queryUpdate = "UPDATE productos SET nombre = ? WHERE codigo = ?"
      argUpdate = [nombre_sql, codigo_sql]
  in do
    [[existeProd]] <- quickQuery' conn querySelect argSelect
    if ((fromSql existeProd) :: Int) == 1
    then do
      filasModificadas <- run conn queryUpdate argUpdate
      commit conn
      return $ toEnum (fromInteger filasModificadas)
    else return False

-- ~ El valor booleano representa si el proveedor fue actualizado o no
actualizarProveedor :: Connection -> Producto -> Text -> IO Bool
actualizarProveedor conn prod proveedor = 
  let codigo_sql = toSql $ codigo prod
      proveedor_sql = toSql proveedor

      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect = [codigo_sql]

      queryUpdate = "UPDATE productos SET proveedor = ? WHERE codigo = ?"
      argUpdate = [proveedor_sql, codigo_sql]
  in do
    [[existeProd]] <- quickQuery' conn querySelect argSelect
    if ((fromSql existeProd) :: Int) == 1
    then do
      filasModificadas <- run conn queryUpdate argUpdate
      commit conn
      return $ toEnum (fromInteger filasModificadas)
    else return False

-- ~ El valor booleano representa si el codigo fue actualizado o no
actualizarCodigo :: Connection -> Producto -> Int32 -> IO Bool
actualizarCodigo conn prod codigo_nuevo = 
  let codigo_sql = toSql $ codigo prod
      codigo_nuevo_sql = toSql codigo_nuevo

      querySelect1 = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect1 = [codigo_sql]

      querySelect2 = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect2 = [codigo_nuevo_sql]

      queryUpdate = "UPDATE productos SET codigo = ? WHERE codigo = ?"
      argUpdate = [codigo_nuevo_sql, codigo_sql]
  in do
    [[existeProd]] <- quickQuery' conn querySelect1 argSelect1
    [[estaCodigoNuevo]] <- quickQuery' conn querySelect2 argSelect2
    if (((fromSql existeProd) :: Int) == 1) && (((fromSql estaCodigoNuevo) :: Int) == 0)
    then do
      filasModificadas <- run conn queryUpdate argUpdate
      commit conn
      return $ toEnum (fromInteger filasModificadas)
    else return False

-- ~ El valor booleano representa si el stock fue actualizado o no
actualizarStock :: Connection -> Producto -> Int32 -> IO Bool
actualizarStock conn prod stock = 
  let codigo_sql = toSql $ codigo prod
      stock_sql = if stock < 0 then SqlNull else toSql stock

      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect = [codigo_sql]

      queryUpdate = "UPDATE productos SET stock = ? WHERE codigo = ?"
      argUpdate = [stock_sql, codigo_sql]
  in do
    [[existeProd]] <- quickQuery' conn querySelect argSelect
    if ((fromSql existeProd) :: Int) == 1
    then do
      filasModificadas <- run conn queryUpdate argUpdate
      commit conn
      return $ toEnum (fromInteger filasModificadas)
    else return False

-- ~ El valor booleano representa si el precio fue actualizado o no
actualizarPrecio :: Connection -> Producto -> Double -> IO Bool
actualizarPrecio conn prod precio = 
  let codigo_sql = toSql $ codigo prod
      precio_sql = toSql precio

      querySelect = "SELECT COUNT(*) FROM productos WHERE codigo = ?"
      argSelect = [codigo_sql]

      queryUpdate = "UPDATE productos SET precio = ? WHERE codigo = ?"
      argUpdate = [precio_sql, codigo_sql]
  in do
    [[existeProd]] <- quickQuery' conn querySelect argSelect
    if ((fromSql existeProd) :: Int) == 1
    then do
      filasModificadas <- run conn queryUpdate argUpdate
      commit conn
      return $ toEnum (fromInteger filasModificadas)
    else return False
