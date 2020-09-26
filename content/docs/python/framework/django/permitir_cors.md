---
title: Permitir CORS
description: Permitir CORS
---

# Objetivo

Habilitar la opción del CORS en Django. Es decir, permitir que Django reciba peticiones desde otros origines distintos.

Para ello hay que instalar en el proyecto de Django el paquete [django-cors-headers](https://github.com/adamchainz/django-cors-headers) mediante el siguiente comando:

```tpl
pip install django-cors-headers
```

A continuación hay que editar el fichero _settings.py_ del proyecto y realizar lo siguiente:

1. En la variable _INSTALLED_APPS_ hay que añadir _'corsheaders'_
2. En la variable _MIDDLEWARE_ hay que añadir antes de _django.middleware.common.CommonMiddleware_ y después de _django.middleware.security.SecurityMiddleware_ lo siguiente:

```tpl
    'corsheaders.middleware.CorsMiddleware',
```

El motivo de poner después del _django.middleware.security.SecurityMiddleware_ es porque estoy usando autentificación con CSRF y tiene que estar después. Y tiene que estar antes de _django.middleware.common.CommonMiddleware_ porque se debe añadir en cabecera del CORS justo antes de cualquier respuesta que pueda dar Django.

3. Se añade las URLS donde va a recibir las peticiones. Se puede hacer para todas las URLs, mirar la documentación del paquete para más información, pero en mi caso lo voy acotar al servidor de React NextJS:

```tpl
# Servidores donde se permite que hagan llamadas a django
CORS_ALLOWED_ORIGINS = [
    "http://localhost:3000"
]

# Servidores donde se permite el funcionamiento de CSRF.
CSRF_TRUSTED_ORIGINS = ['localhost:3000']
```

En la primera variable permitimos llamadas desde cualquier página de NextJS. Y la segunda permitimos autentificación CSRF entre NextJs y Django. Si no se pusiera las cookie de sesión y CSRF no se generarían correctamente.
