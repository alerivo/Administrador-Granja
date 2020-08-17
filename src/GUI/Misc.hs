{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI.Misc where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text)
import Control.Monad (when)

-- ~ window es la ventana principal a la que se asocia el mensaje de error.
mostrarError :: Maybe Gtk.Window -> Text -> Text -> IO ()
mostrarError window titulo msg = do
  dialog <- new Gtk.MessageDialog [ Gtk.dialogUseHeaderBar := 0,
                         Gtk.messageDialogMessageType := Gtk.MessageTypeError,
                         Gtk.messageDialogButtons := Gtk.ButtonsTypeOk,
                         #text := titulo,
                         #secondaryText := msg]
  when (window==Nothing) (putStrLn "nothing")
  Gtk.windowSetTransientFor dialog window
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()

-- ~ window es la ventana principal a la que se asocia el mensaje de error.
mostrarErrorSimple :: Maybe Gtk.Window -> Text -> IO ()
mostrarErrorSimple window msg = do
  dialog <- new Gtk.MessageDialog [ Gtk.dialogUseHeaderBar := 0,
                         Gtk.messageDialogMessageType := Gtk.MessageTypeError,
                         Gtk.messageDialogButtons := Gtk.ButtonsTypeOk,
                         #text := msg ]
  when (window==Nothing) $ putStrLn "nothing"
  Gtk.windowSetTransientFor dialog window
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()
