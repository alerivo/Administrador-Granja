module Inicializador where

import Database.HDBC
import Database.HDBC.Sqlite3

iniciar :: Connection -> IO ()
iniciar conn = do run conn "CREATE TABLE IF NOT EXISTS productos(\
                    \codigo INTEGER PRIMARY KEY,\
                    \nombre TEXT NOT NULL,\
                    \proveedor TEXT,\
                    \precio REAL NOT NULL,\
                    \stock INTEGER)" []
                  run conn "CREATE TABLE IF NOT EXISTS vendidos(\
                    \producto_codigo INTEGER,\
                    \venta_id INTEGER,\
                    \FOREIGN KEY (producto_codigo) REFERENCES productos(codigo)\
                    \ON DELETE RESTRICT \
                    \ON UPDATE RESTRICT \
                    \FOREIGN KEY (venta_id) REFERENCES ventas(id)\
                    \ON DELETE CASCADE \
                    \ON UPDATE RESTRICT)" []
                  run conn "CREATE TABLE IF NOT EXISTS ventas(\
                    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                    \timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP)" []
                  commit conn
