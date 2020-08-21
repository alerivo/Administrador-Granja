{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.Vender where

import GUI.Productos as Productos
import GUI.Misc
import Estructuras
import BibliotecaBD
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Database.HDBC.Sqlite3 (Connection)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Int (Int32)
import Data.Text as T (Text, unpack, pack, init, null, append)
import Text.Read (readMaybe)
import Control.Monad (when)

mostrarPrecio :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
mostrarPrecio treeViewColumn cellRenderer treeModel treeIter = do
  precio_ <- #getValue treeModel treeIter 2 >>= fromGValue :: IO Double
  Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  set cellRendererText [ #text := (pack (show precio_)) ]

mostrarTotal :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
mostrarTotal treeViewColumn cellRenderer treeModel treeIter = do
  precio_ <- #getValue treeModel treeIter 2 >>= fromGValue :: IO Double
  unidades_ <- #getValue treeModel treeIter 3 >>= fromGValue :: IO Int32
  Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  set cellRendererText [ #text := pack $ show $ precio_ * (fromIntegral unidades_) ]

-- ~ Callback para las pulsaciones de teclas en el Stack Vender.
stackVenderCallback builder conn store_por_vender store_productos store_ventas eventKey = do
  Just ingresando_cantidad <- Gtk.builderGetObject builder "ingresando_cantidad" >>= castTo Gtk.Label . fromJust
  Just cantidad <- Gtk.builderGetObject builder "cantidad" >>= castTo Gtk.Label . fromJust
  Just accion <- Gtk.builderGetObject builder "accion" >>= castTo Gtk.Label . fromJust
  cantidad_actual <- Gtk.labelGetText ingresando_cantidad

  key <- get eventKey #keyval
  case key of
    0xff08 -> do  -- BACKSPACE
      when (not $ T.null cantidad_actual) $ Gtk.labelSetText ingresando_cantidad $ T.init cantidad_actual
      return True

    0xffab -> do -- +
      if T.null cantidad_actual
      then Gtk.labelSetText cantidad "1"
      else Gtk.labelSetText cantidad cantidad_actual
      Gtk.labelSetText ingresando_cantidad ""
      Gtk.labelSetMarkup accion "<span background=\"white\">Agregar</span>"
      return True

    0xffad -> do -- -
      if T.null cantidad_actual
      then Gtk.labelSetText cantidad "1"
      else Gtk.labelSetText cantidad cantidad_actual
      Gtk.labelSetText ingresando_cantidad ""
      Gtk.labelSetMarkup accion "<span background=\"red\">Borrar</span>"
      return True

    enter | enter `elem` [0xff8d, 0xff0d] -> do -- ENTER
      ingresando_cantidad_s <- Gtk.labelGetText ingresando_cantidad
      cantidad_s <- Gtk.labelGetText cantidad
      if T.null ingresando_cantidad_s
      then do
        falta_stock <- verificarStocks conn store_por_vender
        if isNothing falta_stock
        then do -- Hay stock de todos los productos
          actualizarStocks conn store_por_vender store_productos
          total_venta <- calcularTotal store_por_vender
          (id_, timestamp_) <- agregarVenta conn total_venta
          id_gvalue <- toGValue id_
          timestamp_gvalue <- toGValue (Just timestamp_)
          total_gvalue <- toGValue total_venta
          Gtk.listStoreInsertWithValuesv store_ventas 0 [0,1,2]
            [id_gvalue,timestamp_gvalue,total_gvalue]
          Just total_ventas <- Gtk.builderGetObject builder "total_ventas" >>= castTo Gtk.Label . fromJust
          total_ventas_s <- Gtk.labelGetText total_ventas
          let total_ventas_int = fromJust ((readMaybe (tail $ unpack total_ventas_s)) :: Maybe Double)
          Gtk.labelSetText total_ventas ("$ " `T.append` (pack $ show $ total_venta + total_ventas_int))
          Gtk.listStoreClear store_por_vender
        else do
          Just nombre_producto_con_faltante <- #getValue store_por_vender (fromJust falta_stock) 0 >>= fromGValue :: IO (Maybe Text)
          window <- Gtk.widgetGetToplevel ingresando_cantidad >>= castTo Gtk.Window
          mostrarErrorSimple window ("No hay stock suficiente del producto " `T.append` nombre_producto_con_faltante)
      else do
        let codigo_ = (read $ unpack ingresando_cantidad_s) :: Int32
            cantidad_int = (read $ unpack cantidad_s) :: Int32
        prod_m <- BibliotecaBD.buscarProducto codigo_ conn
        if isJust prod_m
        then do
          let prod = fromJust prod_m
          accion_actual <- Gtk.labelGetText accion
          iter_m <- GUI.Vender.buscarStore store_por_vender codigo_
          case accion_actual of
            "Agregar" -> do
              if isJust iter_m
              then do
                cant_agregada <- #getValue store_por_vender (fromJust iter_m) 3 >>= fromGValue :: IO Int32
                if maybe True ((cant_agregada + cantidad_int) <= ) (stock prod)
                then do
                  nueva_cantidad_gvalue <- toGValue (cant_agregada + cantidad_int)
                  Gtk.listStoreSetValue store_por_vender (fromJust iter_m) 3 nueva_cantidad_gvalue
                else do
                  window <- Gtk.widgetGetToplevel ingresando_cantidad >>= castTo Gtk.Window
                  mostrarErrorSimple window "No hay stock suficiente de este producto"
              else do
                if maybe True (cantidad_int <= ) (stock prod)
                then do
                  nombre_gvalue <- toGValue $ Just $ nombre prod
                  codigo_gvalue <- toGValue $ codigo prod
                  precio_unit_gvalue <- toGValue (precio prod)
                  cantidad_gvalue <- toGValue cantidad_int

                  Gtk.listStoreInsertWithValuesv store_por_vender (-1) [0,1,2,3]
                    [nombre_gvalue,codigo_gvalue,precio_unit_gvalue,cantidad_gvalue]
                  return ()
                else do
                  window <- Gtk.widgetGetToplevel ingresando_cantidad >>= castTo Gtk.Window
                  mostrarErrorSimple window "No hay stock suficiente de este producto"

            "Borrar" -> do
              when (isJust iter_m) (do
                cant_agregada <- #getValue store_por_vender (fromJust iter_m) 3 >>= fromGValue :: IO Int32
                if cant_agregada <= cantidad_int
                then do
                  Gtk.listStoreRemove store_por_vender (fromJust iter_m)
                  return ()
                else do
                  nueva_cantidad_gvalue <- toGValue (cant_agregada - cantidad_int)
                  Gtk.listStoreSetValue store_por_vender (fromJust iter_m) 3 nueva_cantidad_gvalue)
        else do
          window <- Gtk.widgetGetToplevel ingresando_cantidad >>= castTo Gtk.Window
          mostrarError window "Producto no encontrado" "Posiblemente sea un código erróneo"
      Gtk.labelSetText ingresando_cantidad ""
      Gtk.labelSetText cantidad "1"
      Gtk.labelSetMarkup accion "<span background=\"white\">Agregar</span>"
      return True

    n | n `elem` [0xffb0, 0xffb1, 0xffb2, 0xffb3, 0xffb4, 0xffb5, 0xffb6, 0xffb7, 0xffb8, 0xffb9, 
                0x030, 0x031, 0x032, 0x033, 0x034, 0x035, 0x036, 0x037, 0x038, 0x039] -> do  -- DÍGITOS
      let digito = if key <= 0x039 then key - 0x030 else key - 0xffb0
          digito_s = pack $ show $ digito
      Gtk.labelSetText ingresando_cantidad $ T.append cantidad_actual digito_s
      return True

    _ -> return False

-- ~ Busca un código en la store de productos por vender, devuelve iter a el
-- ~ si lo encuentra y Nothing sino.
buscarStore :: Gtk.ListStore -> Int32 -> IO (Maybe Gtk.TreeIter)
buscarStore store codigo_a_buscar = do
  (set, iter) <- Gtk.treeModelGetIterFirst store
  if set
  then do
    codigo_actual <- #getValue store iter 1 >>= fromGValue :: IO Int32
    if codigo_a_buscar == codigo_actual
    then return $ Just iter
    else buscarStoreAux store codigo_a_buscar iter
  else return Nothing
  where
  buscarStoreAux store codigo_a_buscar iter = do
    set <- Gtk.treeModelIterNext store iter
    if set
    then do
      codigo_actual <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
      if codigo_a_buscar == codigo_actual
      then return $ Just iter
      else buscarStoreAux store codigo_a_buscar iter
    else return Nothing

actualizarTotal :: Gtk.ListStore -> Gtk.Label -> Gtk.TreePath -> Gtk.TreeIter -> IO ()
actualizarTotal store label _ _ = do
  total <- calcularTotal store
  Gtk.labelSetText label $ "$ " `T.append` (pack $ show total)

calcularTotal :: Gtk.ListStore -> IO Double
calcularTotal store = do
  (set, iter) <- Gtk.treeModelGetIterFirst store
  if set
  then do
    precio_unit <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Double
    cantidad <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    calcularTotalAux store iter (precio_unit * (fromIntegral cantidad))
  else return 0
  where
  calcularTotalAux store iter total_acumulado = do
    set <- Gtk.treeModelIterNext store iter
    if set
    then do
      precio_unit <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Double
      cantidad <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
      calcularTotalAux store iter (total_acumulado + precio_unit * (fromIntegral cantidad))
    else return total_acumulado

actualizarTotalSinIter :: Gtk.ListStore -> Gtk.Label -> Gtk.TreePath -> IO ()
actualizarTotalSinIter store label path = do
  fake_iter <- Gtk.newZeroTreeIter
  actualizarTotal store label path fake_iter

-- ~ Verifica que haya suficiente stock de cada producto por vender, en caso que haya
-- ~ devuelve Nothing, sino devuelve iter al primer producto que no haya suficiente stock.
verificarStocks :: Connection -> Gtk.ListStore -> IO (Maybe Gtk.TreeIter)
verificarStocks conn store = do
  (set, iter) <- Gtk.treeModelGetIterFirst store
  if set
  then verificarStocksAux store iter
  else return Nothing
  where
  verificarStocksAux store iter = do
    codigo_ <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    prod_m <- BibliotecaBD.buscarProducto codigo_ conn
    cant_por_vender <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    if isJust prod_m
    then
      if maybe True (>= cant_por_vender) (stock $ fromJust prod_m)
      then do
        set <- Gtk.treeModelIterNext store iter
        if set
        then verificarStocksAux store iter
        else return Nothing
      else return (Just iter)
    else return (Just iter)

-- ~ Actualiza los stocks en la base de datos y en la store de productos en base a los productos
-- ~ que estan en la store_por_vender. Asume que hay suficiente stock de todos los productos.
actualizarStocks :: Connection -> Gtk.ListStore -> Gtk.ListStore -> IO ()
actualizarStocks conn store_por_vender store_productos = do
  (set, iter) <- Gtk.treeModelGetIterFirst store_por_vender
  if set
  then actualizarStocksAux conn store_por_vender store_productos iter
  else return ()
  where
  actualizarStocksAux conn store_por_vender store_productos iter = do
    codigo_ <- Gtk.treeModelGetValue store_por_vender iter 1 >>= fromGValue :: IO Int32
    cant_por_vender <- Gtk.treeModelGetValue store_por_vender iter 3 >>= fromGValue :: IO Int32
    prod_m <- BibliotecaBD.buscarProducto codigo_ conn
    when (isJust prod_m && (isJust $ stock $ fromJust prod_m)) (do
      let stock_actual = fromJust $ stock $ fromJust $ prod_m
          stock_nuevo = stock_actual - cant_por_vender
      actualizarStock conn (fromJust prod_m) stock_nuevo
      iter_productos_m  <- Productos.buscarStore store_productos codigo_
      stock_nuevo_gvalue <- toGValue stock_nuevo
      when (isJust iter_productos_m) (Gtk.listStoreSetValue store_productos (fromJust iter_productos_m) 4 stock_nuevo_gvalue))
    set <- Gtk.treeModelIterNext store_por_vender iter
    if set
    then actualizarStocksAux conn store_por_vender store_productos iter
    else return ()
