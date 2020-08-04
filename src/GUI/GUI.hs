{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.GUI where

import GUI.Productos
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

gladeFile = "assets/gui.glade"
logoFile = Just "assets/logo.png"

gui conn = do
  Gtk.init Nothing

  builder <- Gtk.builderNewFromFile gladeFile
  Just ventana_principal <- Gtk.builderGetObject builder "ventana_principal" >>= castTo Gtk.Window . fromJust
  Just logo <- Gtk.builderGetObject builder "logo" >>= castTo Gtk.Image . fromJust
  Just productos_view <- Gtk.builderGetObject builder "productos_view" >>= castTo Gtk.TreeView . fromJust
  Just buscar_producto <- Gtk.builderGetObject builder "buscar_producto" >>= castTo Gtk.SearchEntry . fromJust
  Just eliminar_producto <- Gtk.builderGetObject builder "eliminar_producto" >>= castTo Gtk.Button . fromJust
  Just agregar_producto <- Gtk.builderGetObject builder "agregar_producto" >>= castTo Gtk.Button . fromJust
  Just ventana_agregar_producto <- Gtk.builderGetObject builder "ventana_agregar_producto" >>= castTo Gtk.Window . fromJust

-- Stack Productos
  -- Lista de productos
  productos_store <- Gtk.listStoreNew [gtypeInt, gtypeString, gtypeString, gtypeDouble, gtypeInt, gtypeBoolean]
  rellenarProductos productos_store conn
  
    -- Busqueda
  Just productos_store_filter <- Gtk.treeModelFilterNew productos_store Nothing >>= castTo Gtk.TreeModelFilter
  Gtk.treeModelFilterSetVisibleColumn productos_store_filter 5
  productos_store_filter_sort_o <- new Gtk.TreeModelSort [ #model := productos_store_filter ]
  
  on buscar_producto #searchChanged $ buscarProducto buscar_producto productos_store

    -- Columnas
  set productos_view [ #model := productos_store_filter_sort_o ]

  columna_nombre <- Gtk.treeViewColumnNew
  set columna_nombre [ #title := "Nombre",
                       #sortColumnId := 1 ]
  renderer_nombre <- Gtk.cellRendererTextNew
  set renderer_nombre [ #editable := True ]
  on renderer_nombre #edited (productoEditado productos_store conn "nombre")
  Gtk.treeViewColumnPackStart columna_nombre renderer_nombre True
  Gtk.treeViewColumnAddAttribute columna_nombre renderer_nombre "text" 1
  Gtk.treeViewAppendColumn productos_view columna_nombre

  columna_proveedor <- Gtk.treeViewColumnNew
  set columna_proveedor [ #title := "Proveedor",
                       #sortColumnId := 2 ]
  renderer_proveedor <- Gtk.cellRendererTextNew
  set renderer_proveedor [ #editable := True ]
  on renderer_proveedor #edited (productoEditado productos_store conn "proveedor")
  Gtk.treeViewColumnPackStart columna_proveedor renderer_proveedor True
  Gtk.treeViewColumnAddAttribute columna_proveedor renderer_proveedor "text" 2
  Gtk.treeViewAppendColumn productos_view columna_proveedor

  columna_codigo <- Gtk.treeViewColumnNew
  set columna_codigo [ #title := "Código",
                       #sortColumnId := 0 ]
  renderer_codigo <- Gtk.cellRendererTextNew
  set renderer_codigo [ #editable := True ]
  on renderer_codigo #edited (productoEditado productos_store conn "codigo")
  Gtk.treeViewColumnPackStart columna_codigo renderer_codigo True
  Gtk.treeViewColumnAddAttribute columna_codigo renderer_codigo "text" 0
  Gtk.treeViewAppendColumn productos_view columna_codigo

  columna_stock <- Gtk.treeViewColumnNew
  set columna_stock [ #title := "Stock",
                       #sortColumnId := 4 ]
  renderer_stock <- Gtk.cellRendererTextNew
  set renderer_stock [ #editable := True ]
  on renderer_stock #edited (productoEditado productos_store conn "stock")
  Gtk.treeViewColumnPackStart columna_stock renderer_stock True
  Gtk.treeViewColumnSetCellDataFunc columna_stock renderer_stock (Just mostrarStock)
  Gtk.treeViewAppendColumn productos_view columna_stock

  columna_precio <- Gtk.treeViewColumnNew
  set columna_precio [ #title := "Precio $",
                       #sortColumnId := 3 ]
  renderer_precio <- Gtk.cellRendererTextNew
  set renderer_precio [ #editable := True ]
  on renderer_precio #edited (productoEditado productos_store conn "precio")
  Gtk.treeViewColumnPackStart columna_precio renderer_precio True
  Gtk.treeViewColumnSetCellDataFunc columna_precio renderer_precio (Just mostrarPrecio)
  Gtk.treeViewAppendColumn productos_view columna_precio

    -- Borrar producto
  on eliminar_producto #clicked (eliminarProductoCallBack conn productos_view productos_store)

    -- Agregar producto
  on agregar_producto #clicked (#showAll ventana_agregar_producto)
  setUpVentanaAgregarProducto builder conn productos_store ventana_agregar_producto

-- Stack Vender

-- Ventana principal
  on ventana_principal #destroy Gtk.mainQuit
  Gtk.windowMaximize ventana_principal
  #showAll ventana_principal

-- Logo
  Gtk.imageSetFromFile logo logoFile

-- Loop principal
  Gtk.main
