---
title: Django
description: Djando y librerías asociadas
---

# Introducción

[Django](https://www.djangoproject.com/) es un framework que permite realizar aplicaciones web de una manera sencilla y rápida.

A esto hay que sumarle [Django Rest Framework](https://www.django-rest-framework.org/), de aquí en adelante DRF, que permite crear servicios REST a través de _Django_

En mi caso quiero usar _Django_ + _Django Rest Framework_ porque me permite publicar servicios web y guardar determinada información en una base de datos (inicialmente estoy usando la que viene por defecto que es _SQLite3_) de una manera bastante sencilla. Ya que usando las vistas de tipo _ViewSet_ DRF se encarga de gestionar todo el CRUD de un modelo de datos sin añadir nada de código.
Además DRF usa la arquitectura MVC. Donde la M son los modelos que se crean en _Django_. La V son las _Views_ de DRF. Y la C es el _serializer_ de DRF. Esto hace posible [crear servicios sin modelo](/docs/python/framework/django/autentificacion_session.md) usando solo las _Views_ y _Serializers_ con DRF. Que no sé si esto con _Django_ se puede hacer.

A nivel de usarlo en nuestras aplicaciones _Django_ se estructura en proyectos y aplicaciones. Y estos se creán dentro del directorio de nuestro proyecto.

Muchos pasos hay que realizarlos desde el terminal de Windows. Que se puede usar tanto el _Anaconda PowerShell_ como el VSCode si lo hemos abiertos a través del _Anaconda Navigator_ con el entorno que queremos ya preseleccionado. El resultado en ambos casos es el mismo

# Instalar las librerías

_Django_ se puede instalar a través de [Anaconda](/docs/python/anaconda.md):

![Libreria Django Rest Framework](/images/python/framework/django/djando_anaconda.png)

_Django Rest Framework_ hay que instalarlo a través del _PiP_:

```tpl
conda activate <entorno>
pip install djangorestframework
```

# Creación del proyecto

Desde el terminal, yo he usado el del VSCode con lo cual lo primero que he hecho es:

```tpl
conda activate <entorno>
```

Aunque tecnicamente estaba en el entorno, ya que lo habia abierto desde el _Anaconda_, con esto me curo en salud.

**IMPORTANTE: Los siguientes pasos hay que hacerlos estando en el directorio de la aplicación. Como he usado el VSCode ya estaba en dicho directorio**

Ahora se crea el proyecto:

```tpl
D:\Users\ivan\anaconda3\envs\ocr\Lib\site-packages\django\bin\django-admin.py startproject rest_api # Importante hay que poner la ruta completa donde esta el django-admin.py
```

**AVISO: Como estaba en el directorio de la aplicación he tenido que poner la ruta completa donde esta el script: djando-admin.py**

El nombre del proyecto es _rest_api_. Con esto nos ha creado un directorio _rest_api_ y a su vez dentro habrá otro directorio con el mismo nombre al cual tenemos que acceder para:

1. Modificar el fichero _settings.py_ que se tiene que modificar:

- Localizar la variable _INSTALLED_APPS_ y añadir el valor _'rest_framework'_ --> Esto nos permite usar la librería para crear servicios Rest
- Localizar la variable _LANGUAGE_CODE_ y cambiar el valor por _'es-es'_
- Localizar la variable _TIME_ZONE_ y cambiar el valor por _Europe/Berlin_

2. Vamos a crear la base de datos inicial. Para ellos hay que volver al directorio del proyecto creado _rest_api_ y lanzar el comando

```tpl
manage.py migrate
```

El fichero _manage.py_ se va utilizar para muchos procesos.

_Django_ a cualquier operación de replicar lo que hagamos en los archivos de los modelos lo llama _migración_.

# Arrancar el servidor

Para arrancar el servidor hay que estar en el directorio del proyecto _rest_api_ y lanzar el siguiente comando

```tpl
manage.py runserver
```

Si accedemos a esta [http://localhost:8000/](http://localhost:8000/) o [http://127.0.0.1:8000/](http://127.0.0.1:8000/) veremos si todo ha ido bien.

La ventana que nos abra con el proceso corriendo no la podemos cerrar ya que sino no funcionara ninguna llamada.

# Panel de administración

Lo bueno de _Django_ es que viene con un panel de control para gestionar usuarios, grupos y modelos. Pero primero hay que generar un superusuario. Para ello desde el directorio del proyecto y ejecutar el siguiente script:

```tpl
manage.py createsuperuser
```

Nos pedirá nombre de usuario, mail y password. Para acceder al panel tan solo hay que acceder a [http://localhost:8000/admin/](http://localhost:8000/admin/)

# Crear aplicación

Será la aplicación donde se crearán los modelos y servicios. Para ellos desde el directorio del proyecto hay que lanzar el siguiente script:

```tpl
manage.py startapp image
```

Donde _image_ es el nombre de nuestra aplicación.

Para que poder usar la aplicación hay que registrarlo en el fichero _settings.py_ de nuestro proyecto y editarlo. En el fichero hay que localizar la variable _INSTALLED_APPS_ y añadir el valor _'image.apps.ImageConfig'_. Donde _image_ es el directorio que ha creado el proyecto, _apps_ es el fichero _apps.py_ y _ImageConfig_ es el nombre del clase dentro del fichero _apps.py_.

# Com replicar los modelos en la base de datos

Como crear modelos hay multitidud de manuales y guias explicandolos. Pero aquí explicamos los pasos para sincronizarlos con la base de datos:

```tpl
import uuid
from django.db import models


class Image(models.Model):
    id = models.UUIDField(auto_created=True, primary_key=True, default=uuid.uuid4,
                          editable=False)
    content = models.BinaryField()
    filename = models.TextField(max_length=100, default='')

```

Una vez creando el modelo hay que lanzar dos script para que sincronicen con nuestro base de datos:

1. Crear una foto de los cambios en los modelos de la aplicación

```tpl
manage.py makemigrations image
```

2. La fotos de los cambios replicarlos a la base de datos:

```tpl
manage.py migrate
```

Si queremos borrar un modelo tan solo hay que borrarlo del fichero _models.py_ y realizar los dos pasos anteriores para sincronizarlos.

## Como administrar los modelos creados

Si accede a la [página de administración](http://localhost:8000/admin/) los modelos no aparecen automáticamente, se tienen que registrar. Para ello hay que ir al directorio donde esta la aplicación y editar el fichero _admin.py_ y añadir la siguiente línea, o líneas según modelos creados:

```tpl
admin.site.register(Image)
```

Donde _Image_ es el nombre del modelo(es una clase) que esta definida en el archivo _models.py_ del mismo directorio.

# Artículos sobre Django

{{<section>}}
