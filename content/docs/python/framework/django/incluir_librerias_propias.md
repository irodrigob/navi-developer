---
title: Incluir librerías propias
description: Incluir librerías propias
---

# Introducción

Django arranca su propio *middleware*, o servidor, donde solo se puede trabajar con los archivos del proyecto y aplicaciones generadas. Pero no podemos usar librerías propias que tengamos la misma carpeta del proyecto, o en una carpeta dentro del proyecto de Django. El motivo es que no se incluyen dentro de las variables entorno y por lo tanto no las reconoce.

La forma de solucionar son dos dependiendo de donde tengamos la carpeta con nuestra libreria.

En ambos casos la solución se realiza en el archivo *setting.py* que esta dentro de la carpeta de configuración del proyecto Django. El motivo de hacerlo en dicho archivo porque es el que se lee cuando arranca el proceso de Django.

**AVISO IMPORTANTE:**

La versión en que la librería esta fuera del proyecto funciona, las peticiones van sin problemas. He detectado que al sincronizar los modelos de las aplicaciones:

```
manage.py makemigrations <app>
```

Da un error que no encuentra la librería y no hay manera de hacerlo funcionar. Dejo como se hace por si en futuro encuentro la manera de corregirlo. Porque a nivel organizativo me gusta más fuera del proyecto.


# Librería fuera de la carpeta del proyecto

Tenemos la siguiente estructura:

Proyecto VS Code
|--- Proyecto Django
|    |--- Aplicación Django
|--- lib
|    |--- img
|    |    |--- image


Hay que añadir las siguientes dos líneas:
```
EXTERNAL_LIBS_PATH = os.path.join(Path(__file__).parent.parent.parent, "lib")
sys.path = ["", EXTERNAL_LIBS_PATH] + sys.path
```
Aquí tenemos:
1. La primera fila creamos una variable donde: 1) encontrados el directorio donde esta el proyecto de VS Code 2) A ese path se le concatena la carpeta, "lib", con la librerias del proyecto
2. Al path de las variables de entonro de python se le añade: el directorio raíz donde esta el archivo, la variable creada y las variables de entorno previa.

Finalmente en nuestro fichero de django la librería se declaría de la siguiente manera:
```tpl
from lib.img import image
```

# Librería dentro de la carpeta de proyecto

** Esta versión es la que uso actualmente porque me funciona con las sincronizaciones de los modelos **

Tenemos la siguiente estructura:

Proyecto VS Code
|--- Proyecto Django
|    |--- Aplicación Django
|    |--- lib
|    |    |--- img
|    |    |    |--- image


Hay que añadir las siguientes dos líneas:
```
EXTERNAL_LIBS_PATH = os.path.join(BASE_DIR, "lib")
sys.path = ["", EXTERNAL_LIBS_PATH] + sys.path
```

Aquí tenemos:
1. Una variable que concatena el valor de la variable anterior + el directorio donde están las librerías
2. Se añade la variable a la variante de entorno
