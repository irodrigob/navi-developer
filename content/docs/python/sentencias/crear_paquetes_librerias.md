---
title: Crear paquetes y librerías
description: Crear paquetes y librerías propias
---

# Introducción

En la siguiente [pagina](/docs/python/sentencias/usar_librerias.md) se explica como usar librerías o paquetes. Aquí se explica como crearnos nuestras propías librerías y/o paquetes para utilizarlos.

Para ello voy a usar un ejemplo de una aplicación que estoy montando. No se si será el mejor ejemplo pero leyendo de varios sitios creo que es una buena estructura.

# Ejemplo

La aplicación, que esta en una fase embrionaria, he creado una paquete cuya carpeta principal es *lib* donde estarán todas las librerias que se usarán en la aplicación. Actualmente tiene los siguientes niveles:
```
lib
|--- __init__.py
|--- image
     |--- __init__.py
     |--- constants.py
     |--- image.py
     |--- imagePreProcessing.py
```

He optado por crear una carpeta llama *lib* y dentro de ella ir creando carpetas por cada tipo distinto de librería, quedando esta manera todo queda más organizado. Si hubiese creado cada librería en el directorio raíz de la aplcación quedaría desorganizado y no sabrías para que sirve cada carpeta.

En los paquetes en cada carpeta tiene que crearse un archivo *__init__.py*  para que cuando hagamos el *import*, python sepa que es una librería. Este fichero puede estar vacio no es necesario introducir nada. 

En mi caso los tengo informado de la siguiente manera:

Para el *__init__.py* del directorio raíz:
```tpl
from lib.image import *
```
Para el *__init__.py* del directorio *lib*:
```tpl
from . import image
from . import imagePreProcessing
from . import constants
```

Esto nos permite simplificar los import en los programa. Ejemplo, gracías al *__init__.py* del directorio raíz se puede hacer esto directamente:
```tpl
import lib
.
.
oImg = lib.image.clImage()
```
Si dicho fichero estuviera vacio el código habría que cambiarlo por esto:
```tpl
from lib.image import image
.
.
oImg = image.clImage()
```

Incluso es posible delimitar que librerías pueden usarse. Si el *__init__.py* del directorio ráiz solo tenemos esto:
```tpl
from lib.image import imagePreProcessing
```

Este trozo de código nos daría error al no encontrar la librería:
```tpl
import lib
.
.
oImg = lib.image.clImage()
```

Aunque, podríamos solucionarlo accedediendo directamente a la librería que queremos: *from lib.image import image*

En librerías pequeñas simplificar las llamadas pueden ser irrelevante porque mirando los directorios podemos encontrar la librería que nos interesa. Pero en libererías más complejas(solo hay que ver la de *numpy*) esto no es una opción, y es más comodo informar en los archivos *__init__.py* que librerías se importan de manera automática al llamar a la librería. De esta manera **se simplifica, y mucho** las llamadas.