# Administrador-Granja
Administrador de granja donde cargar productos y realizar ventas.

Comandos para instalar dependencias:

sudo apt-get install sqlite3

sudo apt-get install libsqlite3-dev

cabal install HDBC --lib

cabal install HDBC-sqlite3 --lib

sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev

cabal install gi-gtk

Comando para compilar:
ghc --make Main.hs -package convertible-1.1.1.0 -package gi-gtk-3.0.35 -package haskell-gi-base-0.24.1 -package conduit-1.3.2 -package haskell-gi-0.24.2
