{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.GUI where

import GUI.Productos as Productos
import GUI.Vender as Vender
import GUI.Ventas as Ventas
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType
import Data.Maybe (fromJust)
import Data.Text as T (append, pack)

gladeFile = "assets/gui.glade"
logoFile = Just "assets/logo.png"

-- ~ Función principal que incializa la GUI.
gui conn = do
  Gtk.init Nothing

  builder <- Gtk.builderNewFromFile gladeFile
  Just ventana_principal <- Gtk.builderGetObject builder "ventana_principal" >>= castTo Gtk.Window . fromJust
  Just logo <- Gtk.builderGetObject builder "logo" >>= castTo Gtk.Image . fromJust
  Just stack_principal <- Gtk.builderGetObject builder "stack_principal" >>= castTo Gtk.Stack . fromJust
  Just productos_view <- Gtk.builderGetObject builder "productos_view" >>= castTo Gtk.TreeView . fromJust
  Just buscar_producto <- Gtk.builderGetObject builder "buscar_producto" >>= castTo Gtk.SearchEntry . fromJust
  Just eliminar_producto <- Gtk.builderGetObject builder "eliminar_producto" >>= castTo Gtk.Button . fromJust
  Just agregar_producto <- Gtk.builderGetObject builder "agregar_producto" >>= castTo Gtk.Button . fromJust
  Just ventana_agregar_producto <- Gtk.builderGetObject builder "ventana_agregar_producto" >>= castTo Gtk.Window . fromJust
  Just box_vender <- Gtk.builderGetObject builder "box_vender" >>= castTo Gtk.Box . fromJust
  Just vender_view <- Gtk.builderGetObject builder "vender_view" >>= castTo Gtk.TreeView . fromJust
  Just total_vender <- Gtk.builderGetObject builder "total_vender" >>= castTo Gtk.Label . fromJust
  Just ventas_view <- Gtk.builderGetObject builder "ventas_view" >>= castTo Gtk.TreeView . fromJust
  Just eliminar_venta <- Gtk.builderGetObject builder "eliminar_venta" >>= castTo Gtk.Button . fromJust
  Just total_ventas <- Gtk.builderGetObject builder "total_ventas" >>= castTo Gtk.Label . fromJust

-- Stack Productos
  -- Lista de productos
  productos_store <- Gtk.listStoreNew [gtypeInt, gtypeString, gtypeString, gtypeDouble, gtypeInt, gtypeBoolean]
  -- Las columnas son: Código | Nombre | Proveedor | Precio | Stock | Visibilidad
  rellenarProductos productos_store conn
  
    -- Busqueda
  Just productos_store_filter <- Gtk.treeModelFilterNew productos_store Nothing >>= castTo Gtk.TreeModelFilter
  Gtk.treeModelFilterSetVisibleColumn productos_store_filter 5
  productos_store_filter_sort_o <- new Gtk.TreeModelSort [ #model := productos_store_filter ]
  
  on buscar_producto #searchChanged $ buscarProductoCallback buscar_producto productos_store

    -- Columnas
  set productos_view [ #model := productos_store_filter_sort_o, #enableSearch := False  ]

  columna_nombre <- Gtk.treeViewColumnNew
  set columna_nombre [ #title := "Nombre", #sortColumnId := 1 ]
  renderer_nombre <- Gtk.cellRendererTextNew
  set renderer_nombre [ #editable := True ]
  on renderer_nombre #edited (productoEditado productos_store conn "nombre")
  Gtk.treeViewColumnPackStart columna_nombre renderer_nombre True
  Gtk.treeViewColumnAddAttribute columna_nombre renderer_nombre "text" 1
  Gtk.treeViewAppendColumn productos_view columna_nombre

  columna_proveedor <- Gtk.treeViewColumnNew
  set columna_proveedor [ #title := "Proveedor", #sortColumnId := 2 ]
  renderer_proveedor <- Gtk.cellRendererTextNew
  set renderer_proveedor [ #editable := True ]
  on renderer_proveedor #edited (productoEditado productos_store conn "proveedor")
  Gtk.treeViewColumnPackStart columna_proveedor renderer_proveedor True
  Gtk.treeViewColumnAddAttribute columna_proveedor renderer_proveedor "text" 2
  Gtk.treeViewAppendColumn productos_view columna_proveedor

  columna_codigo <- Gtk.treeViewColumnNew
  set columna_codigo [ #title := "Código", #sortColumnId := 0 ]
  renderer_codigo <- Gtk.cellRendererTextNew
  set renderer_codigo [ #editable := True ]
  on renderer_codigo #edited (productoEditado productos_store conn "codigo")
  Gtk.treeViewColumnPackStart columna_codigo renderer_codigo True
  Gtk.treeViewColumnAddAttribute columna_codigo renderer_codigo "text" 0
  Gtk.treeViewAppendColumn productos_view columna_codigo

  columna_stock <- Gtk.treeViewColumnNew
  set columna_stock [ #title := "Stock", #sortColumnId := 4 ]
  renderer_stock <- Gtk.cellRendererTextNew
  set renderer_stock [ #editable := True ]
  on renderer_stock #edited (productoEditado productos_store conn "stock")
  Gtk.treeViewColumnPackStart columna_stock renderer_stock True
  Gtk.treeViewColumnSetCellDataFunc columna_stock renderer_stock (Just mostrarStock)
  Gtk.treeViewAppendColumn productos_view columna_stock

  columna_precio <- Gtk.treeViewColumnNew
  set columna_precio [ #title := "Precio $", #sortColumnId := 3 ]
  renderer_precio <- Gtk.cellRendererTextNew
  set renderer_precio [ #editable := True ]
  on renderer_precio #edited (productoEditado productos_store conn "precio")
  Gtk.treeViewColumnPackStart columna_precio renderer_precio True
  Gtk.treeViewColumnSetCellDataFunc columna_precio renderer_precio (Just Productos.mostrarPrecio)
  Gtk.treeViewAppendColumn productos_view columna_precio

    -- Borrar producto
  on eliminar_producto #clicked (eliminarProductoCallback conn productos_view productos_store)

    -- Agregar producto
  on agregar_producto #clicked (#showAll ventana_agregar_producto)
  inicializarVentanaAgregarProducto builder conn productos_store ventana_agregar_producto

-- Stack Vender
  por_vender_store <- Gtk.listStoreNew [gtypeString, gtypeInt, gtypeDouble, gtypeInt]
  -- Las columnas son: Nombre | Código | Precio unitario | Cantidad a vender

  on por_vender_store #rowInserted $ actualizarTotal por_vender_store total_vender
  on por_vender_store #rowChanged $ actualizarTotal por_vender_store total_vender
  on por_vender_store #rowDeleted $ actualizarTotalSinIter por_vender_store total_vender

  Gtk.treeViewGetSelection vender_view >>= (\sel -> set sel [ #mode := Gtk.SelectionModeNone ])

  -- Columnas
  set vender_view [ #model := por_vender_store, #enableSearch := False ]

  columna_nombre_vender <- Gtk.treeViewColumnNew
  set columna_nombre_vender [ #title := "Nombre" ]
  renderer_nombre_vender <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_nombre_vender renderer_nombre_vender True
  Gtk.treeViewColumnAddAttribute columna_nombre_vender renderer_nombre_vender "text" 0
  Gtk.treeViewAppendColumn vender_view columna_nombre_vender

  columna_codigo_vender <- Gtk.treeViewColumnNew
  set columna_codigo_vender [ #title := "Código" ]
  renderer_codigo_vender <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_codigo_vender renderer_codigo_vender True
  Gtk.treeViewColumnAddAttribute columna_codigo_vender renderer_codigo_vender "text" 1
  Gtk.treeViewAppendColumn vender_view columna_codigo_vender

  columna_precio_vender <- Gtk.treeViewColumnNew
  set columna_precio_vender [ #title := "Precio unitario $" ]
  renderer_precio_vender <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_precio_vender renderer_precio_vender True
  Gtk.treeViewColumnSetCellDataFunc columna_precio_vender renderer_precio_vender (Just Vender.mostrarPrecio)
  Gtk.treeViewAppendColumn vender_view columna_precio_vender

  columna_cant_vender <- Gtk.treeViewColumnNew
  set columna_cant_vender [ #title := "Cantidad" ]
  renderer_cant_vender <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_cant_vender renderer_cant_vender True
  Gtk.treeViewColumnAddAttribute columna_cant_vender renderer_cant_vender "text" 3
  Gtk.treeViewAppendColumn vender_view columna_cant_vender

  columna_total_vender <- Gtk.treeViewColumnNew
  set columna_total_vender [ #title := "Total $" ]
  renderer_total_vender <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_total_vender renderer_total_vender True
  Gtk.treeViewColumnSetCellDataFunc columna_total_vender renderer_total_vender (Just Vender.mostrarTotal)
  Gtk.treeViewAppendColumn vender_view columna_total_vender

  ventas_store_DECLARACION_TEMPRANA <- Gtk.listStoreNew [gtypeInt, gtypeString, gtypeDouble]

  -- Originalmente captaba las teclas directamente desde el box_vender, pero cuando tomaba foco vender_view
  -- se dejaba de propagar las teclas enter, de esta manera funciona.
  on ventana_principal #keyPressEvent (\eventKey -> do stack_visible <- Gtk.stackGetVisibleChildName stack_principal
                                                       if maybe False (== "vender") stack_visible
                                                       then stackVenderCallback builder conn por_vender_store productos_store ventas_store_DECLARACION_TEMPRANA eventKey
                                                       else return False)

-- Stack Ventas
  let ventas_store = ventas_store_DECLARACION_TEMPRANA
  -- Las columnas son: ID | TIMESTAMP | Total
  total <- rellenarVentas ventas_store conn
  Gtk.labelSetText total_ventas $ "$ " `T.append` (pack $ show total)

  -- Columnas
  set ventas_view [ #model := ventas_store ]

  columna_id_ventas <- Gtk.treeViewColumnNew
  set columna_id_ventas [ #title := "ID" ]
  renderer_id_ventas <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_id_ventas renderer_id_ventas True
  Gtk.treeViewColumnAddAttribute columna_id_ventas renderer_id_ventas "text" 0
  Gtk.treeViewAppendColumn ventas_view columna_id_ventas

  columna_timestamp_ventas <- Gtk.treeViewColumnNew
  set columna_timestamp_ventas [ #title := "Fecha y hora" ]
  renderer_timestamp_ventas <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_timestamp_ventas renderer_timestamp_ventas True
  Gtk.treeViewColumnAddAttribute columna_timestamp_ventas renderer_timestamp_ventas "text" 1
  Gtk.treeViewAppendColumn ventas_view columna_timestamp_ventas

  columna_total_ventas <- Gtk.treeViewColumnNew
  set columna_total_ventas [ #title := "Total $" ]
  renderer_total_ventas <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart columna_total_ventas renderer_total_ventas True
  Gtk.treeViewColumnSetCellDataFunc columna_total_ventas renderer_total_ventas (Just Ventas.mostrarTotal)
  Gtk.treeViewAppendColumn ventas_view columna_total_ventas

  on eliminar_venta #clicked (eliminarVentaCallback conn ventas_store ventas_view total_ventas)

-- Ventana principal
  on ventana_principal #destroy Gtk.mainQuit
  Gtk.windowMaximize ventana_principal
  #showAll ventana_principal

-- Logo
  Gtk.imageSetFromFile logo logoFile

-- Loop principal
  Gtk.main
