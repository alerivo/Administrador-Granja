{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI where

import Estructuras
import Biblioteca
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Control.Monad.IO.Class
import Data.GI.Base.GType
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base
import Data.Int
import Data.GI.Base.GValue
import Data.Text (pack, unpack)
import Control.Monad (when)
import Data.Text (Text)

gladeFile = "../gui.glade"
logoFile = Just "../logo.png"

gui conn = do
  Gtk.init Nothing

  builder <- Gtk.builderNewFromFile gladeFile
  Just ventana_principal_o <- Gtk.builderGetObject builder "ventana_principal"
  Just ventana_principal <- castTo Gtk.Window ventana_principal_o
  Just logo_o <- Gtk.builderGetObject builder "logo"
  Just logo <- castTo Gtk.Image logo_o
  Just mostrar_productos_o <- Gtk.builderGetObject builder "mostrar_productos"
  Just mostrar_productos <- castTo Gtk.TreeView mostrar_productos_o

  productos_store <- Gtk.listStoreNew [gtypeInt, gtypeString, gtypeString, gtypeDouble, gtypeInt]
  rellenar_productos productos_store conn

  set mostrar_productos [ #model := productos_store ]

  columna_nombre <- Gtk.treeViewColumnNew
  set columna_nombre [ #title := "Nombre",
                       #sortColumnId := 1 ]
  renderer_nombre <- Gtk.cellRendererTextNew
  set renderer_nombre [ #editable := True ]
  on renderer_nombre #edited (producto_editado productos_store conn "nombre")
  Gtk.treeViewColumnPackStart columna_nombre renderer_nombre True
  Gtk.treeViewColumnAddAttribute columna_nombre renderer_nombre "text" 1
  Gtk.treeViewAppendColumn mostrar_productos columna_nombre

  columna_proveedor <- Gtk.treeViewColumnNew
  set columna_proveedor [ #title := "Proveedor",
                       #sortColumnId := 2 ]
  renderer_proveedor <- Gtk.cellRendererTextNew
  set renderer_proveedor [ #editable := True ]
  Gtk.treeViewColumnPackStart columna_proveedor renderer_proveedor True
  Gtk.treeViewColumnAddAttribute columna_proveedor renderer_proveedor "text" 2
  Gtk.treeViewAppendColumn mostrar_productos columna_proveedor

  columna_codigo <- Gtk.treeViewColumnNew
  set columna_codigo [ #title := "Código",
                       #sortColumnId := 0 ]
  renderer_codigo <- Gtk.cellRendererTextNew
  set renderer_codigo [ #editable := True ]
  Gtk.treeViewColumnPackStart columna_codigo renderer_codigo True
  Gtk.treeViewColumnAddAttribute columna_codigo renderer_codigo "text" 0
  Gtk.treeViewAppendColumn mostrar_productos columna_codigo

  columna_stock <- Gtk.treeViewColumnNew
  set columna_stock [ #title := "Stock",
                       #sortColumnId := 4 ]
  renderer_stock <- Gtk.cellRendererTextNew
  set renderer_stock [ #editable := True ]
  Gtk.treeViewColumnPackStart columna_stock renderer_stock True
  Gtk.treeViewColumnAddAttribute columna_stock renderer_stock "text" 4
  -- ~ Gtk.treeViewColumnSetCellDataFunc columna_stock renderer_stock (Just stockCellData)
  Gtk.treeViewAppendColumn mostrar_productos columna_stock

  columna_precio <- Gtk.treeViewColumnNew
  set columna_precio [ #title := "Precio $",
                       #sortColumnId := 3 ]
  renderer_precio <- Gtk.cellRendererTextNew
  set renderer_precio [ #editable := True ]
  Gtk.treeViewColumnPackStart columna_precio renderer_precio True
  Gtk.treeViewColumnAddAttribute columna_precio renderer_precio "text" 3
  Gtk.treeViewAppendColumn mostrar_productos columna_precio

  
  
  
  -- Ventana principal
  on ventana_principal #destroy Gtk.mainQuit
  Gtk.windowMaximize ventana_principal
  #showAll ventana_principal
  -- Logo
  Gtk.imageSetFromFile logo logoFile

  Gtk.main

rellenar_productos store conn =
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
        nombre_ = (Just (fromSql nombre_sql)) :: Maybe String
        proveedor_ = (safeFromSql proveedor_sql) :: Either ConvertError String
        precio_ = (fromSql precio_sql) :: Double
        stock_ = (safeFromSql stock_sql) :: Either ConvertError Int32
        in do
          codigo_gvalue <- toGValue codigo_
          nombre_gvalue <- toGValue nombre_
          proveedor_gvalue <- toGValue (either (\_ -> Nothing) (\s -> Just s) proveedor_)
          precio_gvalue <- toGValue precio_
          stock_gvalue <- toGValue (either (\_ -> -1) id stock_)

          Gtk.listStoreInsertWithValuesv store (-1) [0,1,2,3,4] [codigo_gvalue,nombre_gvalue,proveedor_gvalue,precio_gvalue,stock_gvalue]

          rellenar $ tail productos_

-- ~ stockCellData treeViewColumn cellRenderer treeModel treeIter = do
  -- ~ stock_gvalue <- Gtk.treeModelGetValue treeModel treeIter 4
  -- ~ stock <- (fromGValue stock_gvalue) :: IO Int32
  -- ~ Just cellRendererText <- castTo Gtk.CellRendererText cellRenderer
  -- ~ if stock == -1
  -- ~ then set cellRendererText [ #text := "-" ]
  -- ~ else set cellRendererText [ #text := (pack (show stock)) ]

producto_editado :: Gtk.ListStore -> Connection -> String -> Text -> Text -> IO ()
producto_editado model conn "nombre" path_s nombre = do
  path <- Gtk.treePathNewFromString path_s
  (iterSet, iter) <- Gtk.treeModelGetIter model path
  when (not iterSet) $ return ()
  putStrLn "legaaye"
  -- ~ codigo_ <- Gtk.treeModelGetValue model iter 0 >>= fromGValue :: IO  Int32
  codigo_ <- #getValue model iter 0 >>= fromGValue :: IO Int32
  let prod = Producto {codigo = fromIntegral codigo_}
  actualizarNombre conn prod (unpack nombre)
  return ()
