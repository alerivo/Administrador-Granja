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

En los siguientes enlaces hay documentación sobre los paquetes mas usados:
* ![HDBC](https://hackage.haskell.org/package/HDBC-2.4.0.3/docs/Database-HDBC.html)
* ![GTK3](https://developer.gnome.org/gtk3/stable/)
* ![gi-gtk](https://hackage.haskell.org/package/gi-gtk-3.0.33)

## Base de datos
El programa utiliza una base de datos para llevar registro de todos los productos y las ventas realizadas.

La siguiente imagen muestra el diagrama entidad-relación utilizado por la misma:

![diagrama e/r](/assets/diagrama_entidad_relacion.png)

Y la siguiente imagen muestra el pasaje a tablas:

![tablas](/assets/tablas.png)

## Posibles mejoras:
* Asignar teclas para seleccionar cada Stack (por ejemplo F1, F2 y F3)
* Mejorar el historial de ventas: que quede registrado que productos se vendieron.
* Mejorar la visualización de las ventas: por ejemplo poder ver las ventas por día/mes/año.
* Stock con cantidades reales en vez de enteras.
* Cuando se concreta una venta, para actualizar el stock de los productos en la store de productos la misma se recorre una a una por cada producto. Podría ser un proceso lento si hay muchos productos. Evitar esto sería una mejora.