---
title: Servicios con ficheros
description: Envio, recepción y borrado de ficheros
---

# Pre-Introducción :-)

Antes de nada explicar como se guardan los ficheros usando *Django*. Los ficheros en vez de guardarse en binario en un campo de la base de datos aquí por lo que que leído, se guardan en un directorio interno y en base de datos se guarda la ruta a dicha fichero. Por lo cual, cuando el servicio que devuelve el fichero no devuelve el contenido. Devuelve la url directamente. 

En SAP, que es lo que más experiencia tengo, los ficheros solo guardar el binario en una tabla. Pero si que es verdad que cuando lo devuelve lo hago a través de una URL. Esto es mucho más óptimo para todos, tanto backend como frontend.

Otra nota importante. Es que cuando se lanza el servicio de borrado, se borra los datos en el modelo pero no el directorio que se guarda. Esto he léido es para garantizar integridad y bla bla. Yo lo veo, que en ese caso se tendría que tener un proceso de borrado de ficheros huerfanos. 

Otro punto es que si en el mismo path donde se guardan las imagenes hay un fichero con el mismo nombre le añade un sufijo final para distinguirlo. Eso esta muy bien.

A modo de prueba personal, he intentado guardarme el binario en un campo del modelo pero me ha sido imposible conseguir. Consigo, eso creo, recuperar el binario en la petición HTTP pero no consigo grabarla en el modelo.

# Introducción

El objetivo es explicar como funcionan los servicios que operan con ficheros. Esto por si solo se puede ir haciendo con la documentación oficial, y algun que otro ejemplo encontrado por ahí. Pero hay dos cosas que me han obligado a hacerlo a medida:

1. Cuando se recibe un fichero quiero guardar el nombre del fichero y tipo de fichero. 
2. Quiero borrar el fichero en el directorio donde se ha guardado cuando se lanze el proceso de borrado.

Las vistas del ejemplo son las de tipo *viewSets* de *Django Rest Framework*, de aquí en adelante DRF. El mótivo es que este tipo de vistas implementan de manera automática los procesos CRUD sin necesidad de añadir código. Y a mi me interesa sobrecargar la creación y borrado, pero el de lectura y lista quiero seguir usando los que me ofrece DRF.

Como apunte final las pruebas del servicio las he hecho con el postman.

La estructura de Django es: 
* Nombre de proyecto: backend
* Nombre de aplicación: invoices

Con lo que a nivel de directorio tenemos:

backend
|---backend -> Donde esta la configuración del proyecto
|---invoices
|---media -> Carpeta donde se guardará los archivos

# Configuración de Django

## Configuración general

Para poder usar ficheros a Django hay que decirle donde lo tiene que guardar. Para eso hay que localizar el fichero *settings.py* del proyecto (que esta en la carpeta, del mismo nombre, dentro de la carpeta del proyecto). En ese fichero hay que añadir la siguiente línea:

```tpl
# Directorio donde se guardan los archivos de imagen, video, eetc..
MEDIA_URL = '/media/'
MEDIA_ROOT = os.path.join(BASE_DIR, "media")
```
El primero indica la carpeta dentro del proyecto que se creará (no hace falta crearla de antemano, se crea de manera automática). Y al segunda el path completo donde estará la carpeta en el sistema operativo. Esto sirve para que cuando tengamos el objeto de campo *ImageField* habrá un atributo donde esta la ruta completa del fichero. Útil para borrar el fichero, leerlo, etc.

## Configuración de URLs

En el fichero *urls.py* del proyecto hay que añadir lo siguiente a las URLs que tengamos configuradas:

```tpl
from django.contrib import admin
from django.urls import path, include
from invoices import urls
from django.conf import settings
from django.conf.urls.static import static

urlpatterns = [
    path('admin/', admin.site.urls)
] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
```

Se añade el *+ static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)* para añadir en los archivos estáticos del proyecto el path de las imagenes


# Ejemplo de recepción del fichero

## Modelo de datos

En el fichero *models.py* de la aplicación se añade el siguiente modelo:

```tpl
import uuid
from django.db import models
from django.utils import timezone


class invoiceImage(models.Model):
    id_file = models.UUIDField(
        default=uuid.uuid4, primary_key=True, editable=False)
    file_name = models.CharField(max_length=100, default='')
    file_type = models.CharField(max_length=100, default='')
    created_date = models.DateTimeField(default=timezone.now)
    file_path = models.ImageField(default='', upload_to='invoice/image/')
```
Campos:
* id_file --> ID único que se informa al grabar
* file_name --> nombre del fichero que viene en el servicio
* file_type --> Es el *content-type* del fichero del servicio
* created_date --> Fecha de creación del fichero
* file_path --> URL donde estará la imagen. Para que funcione la grabación de la imagen el tipo de fichero debe ser *ImageField*. Hay otro que se llama *FileField*, que funciona lo mismo(eso creo) pero con ficheros de todo tipo. En este tipo de fichero es posible añadir el path (que se añade al indicado de la configuración) donde se guarda el fichero. Esto permite clasificar las imagenes por directorios.

## Serializers
Para mi es el controlador, es que recibe los datos del modelo y los devuelve en formato JSON a la vista. El código que le he puesto en el fichero *serializers.py* es el siguiente:

```tpl
from rest_framework import serializers
from . import models


class invoiceImageSerializer(serializers.ModelSerializer):
    class Meta:
        model = models.invoiceImage
        fields = ('id_file', 'file_name', 'created_date',
                  'file_path', 'file_type')
```

Es muy sencillo en la variable *model* se indica el modelo definido en el paso anterior. Y en *fields* los campos que queremos que vea la vista. 

## Views

Aquí esta la parte más importante.

```tpl
from rest_framework import parsers
from rest_framework import response
from rest_framework import status
from rest_framework import viewsets
from django.http import Http404
import os
from . import models
from . import serializers

class invoiceImageViewSet(viewsets.ModelViewSet):
    queryset = models.invoiceImage.objects.all()
    serializer_class = serializers.invoiceImageSerializer
    parser_classes = [parsers.MultiPartParser]

    """
    Este método se llamada al hacer la llamado POST 
    """

    def create(self, request):
        # Se recupera el objeto fichero que esta parseado por django rest framework
        file_obj = request.data['content-file']
        # Se inicializa el serializer pasando los valores del fichero: nombre, tipo y el propio objeto.
        serializer = self.serializer_class(
            data={'file_name': request.data['content-file'].name, 'file_path': file_obj, 'file_type': request.data['content-file'].content_type})

        # Si los datos son correctos se graba el modelo, donde se autoinformará los campos automáticos como el: id, fecha de creación, etc.
        if serializer.is_valid():
            serializer.save()
            return response.Response(serializer.data, status=status.HTTP_201_CREATED)
        return response.Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

```

Cosas a tener en cuenta:

1. Las variables *queryset* y *serializer_class* se ponen siempre. Es para que el resto de método de consulta funcionen
2. La variable *parser_classes* hay que poner la que se indica. En los ejemplos de DRF aparece la *FileUploadParser* pero a mi no me funciona. 
3. Tal como se explica en las [equivalencias HTTP](/docs/python/framework/django/equivalencia_metodos_http_drf.md) el método que se sobrecarga el *create*. 
4. En el apartado de testeo se verá pero la variable donde viaja el fichero se llama *content-file*. Iba usar la *file* pero el *Postman* me da un warning y el fichero no viajaba.
5. Una vez grabado los datos en *serializer.data* se pueden acceder a todos los campos incluídos los que se autorellenan.

## URLs

En el archivo *urls.py* de la aplicación hay que registrar la nueva vista:
```tpl

from rest_framework import routers
from . import views

router = routers.DefaultRouter()
router.register(r'invoiceImage', views.invoiceImageViewSet)
```

## Testeo

Las pruebas se han hecho con el postman y esta es la llamada:

![Enviar fichero](/images/python/framework/django/enviar_fichero_servicio.png)

El resultado:

```
{
    "id_file": "8fa3b1bc-975d-472a-b0ae-7f028fc26769",
    "file_name": "DSC05753.JPG",
    "created_date": "2020-08-22T18:39:55.165869+02:00",
    "file_path": "/media/invoice/image/DSC05753_apEwEM9.JPG",
    "file_type": "image/jpeg"
}
```

# Ejemplo de borrado del fichero

Siguiendo el ejemplo anterior vamos añadir como procesar el borrado para que nos haga el borrado físico de la imagen, aparte del borrado en el modelo. Nos basaremos en el ejemplo anterior por lo que solo hay que modificar el fichero *views.py* para añadir el siguiente método

```tpl
    def destroy(self, request, pk=None):
        try:

            # Obtengo el objeto pasado
            image = self.get_object()

            # Se borra la imagen
            image.delete()

            # Finalmente se borra la imagen fisica
            os.remove(image.file_path.path)

        except Http404:  # Si no existe se lanza exepción
            raise Http404

        # Si no hay errores entonces se envia que ha ido bien el borrado
        return response.Response(status=status.HTTP_204_NO_CONTENT)
```

Cosas a tener en cuenta:

1. La equivalencia de método HTTP DELETE es la función *destroy*
2. El *self.get_object()* nos recupera los datos del ID de la imagen pasada por parámetro. Si no existe se devuelve un error 404
3. Se hace primero el borrado en el modelo por si hay cualquier error o excepción. Y finalmente el borrado físico. En el atributo *file_path.path* tenemos el path absoluto donde esta la imagen.
4. Si todo va bien se devuelve el código HTTP 204 que no hay contenido.

Para probarlo en *Postman* sería con la siguiente llamada:

![Borrado fichero](/images/python/framework/django/borrar_fichero_servicio.png)
