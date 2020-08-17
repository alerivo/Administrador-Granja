module Inicializador where

import Database.HDBC (run, commit)
import Database.HDBC.Sqlite3 (Connection)

-- ~ Crea las tablas en caso de que no esten creadas.
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
