{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.Productos where

import GUI.Misc
import Estructuras
import Biblioteca
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType
import Database.HDBC (fromSql, safeFromSql, quickQuery')
import Database.HDBC.Sqlite3 (Connection)
import Data.Convertible.Base (ConvertError)
import Data.Maybe (fromJust, isNothing, isJust, maybe)
import Data.Int (Int32)
import Data.Text (Text, pack, unpack, isInfixOf)
import Text.Read (readMaybe)
import Control.Monad (when)

rellenarProductos :: Gtk.ListStore -> Connection -> IO ()
rellenarProductos store conn =
  let
    query = "SELECT codigo,nombre,proveedor,precio,stock FROM productos"
  in do
    productos <- quickQuery' conn query []
    rellenar productos
    where
    rellenar productos_ =
      if productos_ == []
      then return ()
      else let
        [codigo_sql, nombre_sql, proveedor_sql, precio_sql, stock_sql] = head productos_
        codigo_ = (fromSql codigo_sql) :: Int32
        nombre_ = (Just (fromSql nombre_sql)) :: Maybe Text
        proveedor_ = (safeFromSql proveedor_sql) :: Either ConvertError Text
        precio_ = (fromSql precio_sql) :: Double
        stock_ = (safeFromSql stock_sql) :: Either ConvertError Int32
        in do
          codigo_gvalue <- toGValue codigo_
          nombre_gvalue <- toGValue nombre_
          proveedor_gvalue <- toGValue (either (\_ -> Just "") Just proveedor_)
          precio_gvalue <- toGValue precio_
          stock_gvalue <- toGValue (either (\_ -> -1) id stock_)
          visible_gvalue <- toGValue True

          Gtk.listStoreInsertWithValuesv store (-1) [0,1,2,3,4,5]
            [codigo_gvalue,nombre_gvalue,proveedor_gvalue,precio_gvalue,stock_gvalue,visible_gvalue]

          rellenar $ tail productos_

mostrarStock :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
mostrarStock treeViewColumn cellRenderer treeModel treeIter = do
  stock_ <- #getValue treeModel treeIter 4 >>= fromGValue :: IO Int32
  Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  if stock_ == -1
  then set cellRendererText [ #text := "-" ]
  else set cellRendererText [ #text := (pack (show stock_)) ]

mostrarPrecio :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
mostrarPrecio treeViewColumn cellRenderer treeModel treeIter = do
  precio_ <- #getValue treeModel treeIter 3 >>= fromGValue :: IO Double
  Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  set cellRendererText [ #text := (pack (show precio_)) ]

productoEditado :: Gtk.ListStore -> Connection -> Text -> Text -> Text -> IO ()
productoEditado model conn "nombre" path_s nombre = do
  path <- Gtk.treePathNewFromString path_s
  (iter_set, iter) <- Gtk.treeModelGetIter model path
  when iter_set (do
    codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
    let prod = Producto {codigo = fromIntegral codigo_}
    success <- actualizarNombre conn prod nombre
    when success   $ toGValue (Just nombre) >>= #setValue model iter 1)

productoEditado model conn "proveedor" path_s proveedor = do
  path <- Gtk.treePathNewFromString path_s
  (iter_set, iter) <- Gtk.treeModelGetIter model path
  when iter_set (do
    codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
    let prod = Producto {codigo = fromIntegral codigo_}
    success <- actualizarProveedor conn prod proveedor
    when success $ toGValue (Just proveedor) >>= #setValue model iter 2)

productoEditado model conn "codigo" path_s codigo =
  case (readMaybe (unpack codigo)) :: Maybe Int32 of
    Nothing -> return ()
    Just codigo_nuevo -> do 
      path <- Gtk.treePathNewFromString path_s
      (iter_set, iter) <- Gtk.treeModelGetIter model path
      when iter_set (do
        codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
        let prod = Producto {codigo = fromIntegral codigo_}
        success <- actualizarCodigo conn prod codigo_nuevo
        when success $ toGValue codigo_nuevo >>= #setValue model iter 0)

productoEditado model conn "stock" path_s stock = do
  path <- Gtk.treePathNewFromString path_s
  (iter_set, iter) <- Gtk.treeModelGetIter model path
  when iter_set (do
    codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
    let prod = Producto {codigo = fromIntegral codigo_}
        stock_nuevo = (readMaybe (unpack stock)) :: Maybe Int32
    if isNothing stock_nuevo || (isJust stock_nuevo && (fromJust stock_nuevo) < 0)
    then do
      success <- actualizarStock conn prod (-1)
      when success $ toGValue ((-1)::Int32) >>= #setValue model iter 4
    else do
      success <- actualizarStock conn prod (fromJust stock_nuevo)
      when success $ toGValue (fromJust stock_nuevo) >>= #setValue model iter 4)

productoEditado model conn "precio" path_s precio_s =
  case (readMaybe (unpack precio_s)) :: Maybe Double of
    Nothing -> return ()
    Just precio -> when (precio >= 0) (do
      path <- Gtk.treePathNewFromString path_s
      (iter_set, iter) <- Gtk.treeModelGetIter model path
      when iter_set (do
        codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
        let prod = Producto {codigo = fromIntegral codigo_}
        success <- actualizarPrecio conn prod precio
        when success $ toGValue precio >>= #setValue model iter 3))

buscarProducto :: Gtk.SearchEntry -> Gtk.ListStore -> IO ()
buscarProducto buscar_producto productos_store = do
  busqueda <- get buscar_producto #text
  Gtk.treeModelForeach productos_store (actualizarVisibilidad busqueda)
  where actualizarVisibilidad busqueda model path iter = do
          codigo_ <- Gtk.treeModelGetValue model iter 0 >>= fromGValue :: IO Int32
          Just nombre_ <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO (Maybe Text)
          Just proveedor_ <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO (Maybe Text)
          Just model_list <- castTo Gtk.ListStore model
          let codigo_inffix = isInfixOf busqueda (pack (show codigo_))
              nombre_inffix = isInfixOf busqueda nombre_
              proveedor_inffix = isInfixOf busqueda proveedor_
          if codigo_inffix || nombre_inffix || proveedor_inffix
          then do
            visibilidad <- toGValue True
            Gtk.listStoreSet model_list iter [5] [visibilidad]
          else do
            visibilidad <- toGValue False
            Gtk.listStoreSet model_list iter [5] [visibilidad]
          return False

eliminarProductoCallBack :: Connection -> Gtk.TreeView -> Gtk.ListStore -> IO ()
eliminarProductoCallBack conn productos_view productos_store = do
  selection <- Gtk.treeViewGetSelection productos_view
  (hay_seleccion, model, iter) <- Gtk.treeSelectionGetSelected selection
  if hay_seleccion
  then do
    codigo_ <- Gtk.treeModelGetValue model iter 0 >>= fromGValue :: IO Int32
    let prod = Producto {codigo = fromIntegral codigo_}
    success <- eliminarProducto prod conn
    path <- Gtk.treeModelGetPath model iter
    (success2, iter2) <- Gtk.treeModelGetIter productos_store path
    when (success && success2) $ aplicar (Gtk.listStoreRemove productos_store iter2)
  else return ()

setUpVentanaAgregarProducto :: Gtk.Builder -> Connection -> Gtk.ListStore -> Gtk.Window -> IO ()
setUpVentanaAgregarProducto builder conn store ventana = do
  on ventana #deleteEvent (\_ -> Gtk.widgetHide ventana >>= (\_ -> return True))
  
  Just cancelar <- Gtk.builderGetObject builder "cancelar_agregar_producto" >>= castTo Gtk.Button . fromJust
  Just agregar <- Gtk.builderGetObject builder "aceptar_agregar_producto" >>= castTo Gtk.Button . fromJust

  on cancelar #clicked $ Gtk.widgetHide ventana

  on agregar #clicked $ agregarAux builder conn store ventana

  return ()

agregarAux :: Gtk.Builder -> Connection -> Gtk.ListStore -> Gtk.Window -> IO ()
agregarAux builder conn store ventana = do
  Just nombre_e <- Gtk.builderGetObject builder "entry_nombre_agregar_producto" >>= castTo Gtk.Entry . fromJust
  Just proveedor_e <- Gtk.builderGetObject builder "entry_proveedor_agregar_producto" >>= castTo Gtk.Entry . fromJust
  Just codigo_e <- Gtk.builderGetObject builder "entry_codigo_agregar_producto" >>= castTo Gtk.Entry . fromJust
  Just precio_e <- Gtk.builderGetObject builder "entry_precio_agregar_producto" >>= castTo Gtk.Entry . fromJust
  Just stock_e <- Gtk.builderGetObject builder "entry_stock_agregar_producto" >>= castTo Gtk.Entry . fromJust

  nombre_s <- Gtk.entryGetText nombre_e
  proveedor_s <- Gtk.entryGetText proveedor_e
  codigo_s <- Gtk.entryGetText codigo_e
  precio_s <- Gtk.entryGetText precio_e
  stock_s <- Gtk.entryGetText stock_e

  let codigo_m = readMaybe (unpack codigo_s) :: Maybe Int32
      precio_m = readMaybe (unpack precio_s) :: Maybe Double
      stock_m = readMaybe (unpack stock_s) :: Maybe Int32

      error1 = if nombre_s == "" then True else False
      error2 = if isNothing codigo_m then True else False
      error3 = if isNothing precio_m || (fromJust precio_m) < 0 then True else False

  if error1 || error2 || error3
  then mostrarError "El producto debe contener un nombre, un código numerico y un precio mayor o igual a 0."
  else do
    let codigo_ = fromJust codigo_m
        nombre_ = Just nombre_s
        proveedor_ = if proveedor_s == "" then Nothing else Just proveedor_s

        prod = Producto {codigo = codigo_,
                         nombre = nombre_,
                         proveedor = proveedor_,
                         precio = precio_m,
                         stock = stock_m}

    success <- agregarProducto prod conn

    if success
    then do
      codigo_gvalue <- toGValue codigo_
      nombre_gvalue <- toGValue nombre_
      proveedor_gvalue <- toGValue (maybe (Just "") Just proveedor_)
      precio_gvalue <- toGValue $ fromJust precio_m
      stock_gvalue <- toGValue (maybe (-1) id stock_m)
      visible_gvalue <- toGValue True

      Gtk.listStoreInsertWithValuesv store (-1) [0,1,2,3,4,5]
        [codigo_gvalue,nombre_gvalue,proveedor_gvalue,precio_gvalue,stock_gvalue,visible_gvalue]

      Gtk.widgetHide ventana
      Gtk.entrySetText nombre_e ""
      Gtk.entrySetText proveedor_e ""
      Gtk.entrySetText codigo_e ""
      Gtk.entrySetText precio_e ""
      Gtk.entrySetText stock_e ""

      return ()
    else mostrarError "Probablemente ya existe un producto con el mismo código."
