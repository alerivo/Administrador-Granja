# Administrador-Granja
Administrador de granja donde cargar productos, realizar ventas y visualizarlas.

Utiliza [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) para manejar el proyecto.

Comandos para instalar dependencias:
```sudo apt-get install sqlite3
sudo apt-get install libsqlite3-dev
stack install HDBC
stack install HDBC-sqlite3
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
stack install gi-gtk
```

Comando para compilar y ejecutar:
`stack build && stack exec Administrador-Granja-exe`

## Base de datos
El programa utiliza una base de datos para llevar registro de todos los productos y las ventas realizadas.

La siguiente imagen muestra el diagrama entidad-relación utilizado por la misma:

![diagrama e/r](/assets/diagrama_entidad_relacion.png)

Y la siguiente imagen muestra el pasaje a tablas:

![tablas](/assets/tablas.png)

## Posibles mejoras:
* Evitar la edición de un producto que esta por venderse.
* Asignar teclas para seleccionar cada Stack (por ejemplo F1, F2 y F3)
* Mejorar el historial de ventas: que quede registrado que productos se vendieron.
* Mejorar visualización de las ventas: por ejemplo poder ver las ventas por día/mes/año.
