{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.Misc where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text)

mostrarError :: Text -> IO ()
mostrarError msg = do
  dialog <- new Gtk.MessageDialog [ Gtk.dialogUseHeaderBar := 0,
                         Gtk.messageDialogMessageType := Gtk.MessageTypeError,
                         Gtk.messageDialogButtons := Gtk.ButtonsTypeOk,
                         #text := "Error al agregar el producto",
                         #secondaryText := msg]
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()

-- Aplica una operacion monadica y desecha lo que retorna.
-- Esta función es util para usar la función when
aplicar :: IO a -> IO ()
aplicar a = a >>= \_ -> return ()
