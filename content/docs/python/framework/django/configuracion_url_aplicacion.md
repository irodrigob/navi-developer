---
title: Configuración URLs aplicación
description: Configuración URLs aplicación
---

# Introducción

Cada aplicación dentro de un proyecto en *Django* tiene su propio archivo de URLs, *urls.py* donde se van incluyendo las vistas que se van añadiendo. Aquí explicaremos como configurar dicho archivo tanto a nivel de proyecto como de aplicación.

Hay que saber que el punto de acceso de los servicios será el archivo *urls.py* a nivel de proyecto. Por ello yo lo que hago es:

* Uso el fichero *urls.py* a nivel de aplicación para indicar sus vistas
* Dentro del fichero *urls.py* a nivel de proyecto incluyo el fichero homólogo a nivel de aplicación.

Esta solución la veo la más limpia y estructurada.

# URLs a nivel de aplicación
Si se usan vistas de tipo *ViewSet* la manera más simple de hacerlo es a través de los routers: 

```tpl
from rest_framework import routers
from . import views

router = routers.DefaultRouter()
router.register(r'invoice/image', views.invoiceImageViewSet)
```

# URls a nivel de proyecto

El fichero actual que tengo es este:

```tpl
from django.contrib import admin
from django.urls import path, include
from invoices import urls
from django.conf import settings
from django.conf.urls.static import static
from . import views

urlpatterns = [
    path('admin/', admin.site.urls),
    path(r'api/', include(urls.router.urls)),
    path(r'login', views.LoginView.as_view(), name='login'),
    path(r'logout', views.LogoutView.as_view(), name='logout')
] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
```
el path de *admin* ya viene por defecto y le he añadido:

* en el path *api* será el punto de acceso a las vistas de las distintas aplicaciones que pueda ir creando
* Los servicios de *login* y *logout* al ser globales al proyecto están definidos directamente en el proyecto.
* Se le añade los path para los archivos estáticos.


# URL de llamada

La URL para llamar a un servicio de la aplicación como el configurado sería:
```
http://localhost:8000/api/invoice/image/
```

Para llamar a un servicio del proyecto sería:

```
http://localhost:8000/login
```
