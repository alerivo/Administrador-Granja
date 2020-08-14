module Inicializador where

import Database.HDBC (run, commit)
import Database.HDBC.Sqlite3 (Connection)

iniciar :: Connection -> IO ()
iniciar conn = do run conn "CREATE TABLE IF NOT EXISTS productos(\
                    \codigo INTEGER PRIMARY KEY,\
                    \nombre TEXT NOT NULL,\
                    \proveedor TEXT,\
                    \precio REAL NOT NULL,\
                    \stock INTEGER)" []
                  run conn "CREATE TABLE IF NOT EXISTS ventas(\
                    \id INTEGER PRIMARY KEY,\
                    \timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,\
                    \total REAL NOT NULL)" []
                  commit conn
