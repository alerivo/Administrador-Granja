# Administrador-Granja
Administrador de granja donde cargar productos, realizar ventas y visualizarlas.

Comandos para instalar dependencias:
```sudo apt-get install sqlite3
sudo apt-get install libsqlite3-dev
cabal install HDBC --lib
cabal install HDBC-sqlite3 --lib
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
cabal install gi-gtk
```

Comando para compilar y ejecutar:
`stack build && stack exec Administrador-Granja-exe`

## To Do:
* Evitar la edicion de un producto que esta por venderse.
* Asignar teclas para cada Stack (por ejemplo F1, F2 y F3)
* Mejorar el historial de ventas: que quede registrado que productos se vendieron.
* Mejorar visualización de las ventas: por ejemplo poder ver las ventas por día/mes/año.
