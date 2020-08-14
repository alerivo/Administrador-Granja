{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.Ventas where

-- ~ import GUI.Misc
-- ~ import Estructuras
import Biblioteca
import GI.Gtk as Gtk
import Data.GI.Base
-- ~ import Data.GI.Base.GType
import Database.HDBC (fromSql, quickQuery')
import Database.HDBC.Sqlite3 (Connection)
-- ~ import Data.Convertible.Base (ConvertError)
import Data.Maybe (fromJust)
import Data.Int (Int32)
import Data.Text as T (Text, pack, unpack, append)
import Text.Read (readMaybe)
import Control.Monad (when, void)

rellenarVentas :: Gtk.ListStore -> Connection -> IO Double
rellenarVentas store conn =
  let
    query = "SELECT id,timestamp,total FROM ventas"
  in do
    ventas <- quickQuery' conn query []
    rellenar ventas 0
    where
    rellenar ventas total_acumulado =
      if ventas == []
      then return total_acumulado
      else let
        [id_sql, timestamp_sql, total_sql] = head ventas
        id_ = (fromSql id_sql) :: Int32
        timestamp_ = (Just (fromSql timestamp_sql)) :: Maybe Text
        total_ = (fromSql total_sql) :: Double
        in do
          id_gvalue <- toGValue id_
          timestamp_gvalue <- toGValue timestamp_
          total_gvalue <- toGValue total_

          Gtk.listStoreInsertWithValuesv store 0 [0,1,2]
            [id_gvalue,timestamp_gvalue,total_gvalue]

          rellenar (tail ventas) (total_acumulado + total_)

mostrarTotal :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
mostrarTotal treeViewColumn cellRenderer treeModel treeIter = do
  total_ <- #getValue treeModel treeIter 2 >>= fromGValue :: IO Double
  Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  set cellRendererText [ #text := pack $ show total_ ]

eliminarVentaCallBack :: Connection -> Gtk.ListStore -> Gtk.TreeView -> Gtk.Label -> IO ()
eliminarVentaCallBack conn store view total_ventas = do
  selection <- Gtk.treeViewGetSelection view
  (hay_seleccion, model, iter) <- Gtk.treeSelectionGetSelected selection
  if hay_seleccion
  then do
    id_ <- Gtk.treeModelGetValue model iter 0 >>= fromGValue :: IO Int32
    total_ <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Double
    eliminarVenta conn id_
    path <- Gtk.treeModelGetPath model iter
    (success, iter2) <- Gtk.treeModelGetIter store path
    when success $ void $ Gtk.listStoreRemove store iter2
    total_ventas_s <- Gtk.labelGetText total_ventas
    let total_ventas_int = fromJust ((readMaybe (tail $ unpack total_ventas_s)) :: Maybe Double)
    Gtk.labelSetText total_ventas ("$ " `T.append` (pack $ show $ total_ventas_int - total_))
  else return ()
