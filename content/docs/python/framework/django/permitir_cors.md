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

A continuación hay que editar el fichero "settings.py" del proyecto y realizar lo siguiente:

1. En la variable *INSTALLED_APPS* hay que añadir *'corsheaders'*
2. En la variable *MIDDLEWARE* hay que añadir al **principio de todo** lo siguiente:

```tpl
    'corsheaders.middleware.CorsMiddleware',
    'django.middleware.common.CommonMiddleware',
```

El middleware, al menos en mi caso, *django.middleware.common.CommonMiddleware* ya estaba definido pero lo he movido a la segunda posición. El motivo de ponerlo al principio estos middleware es para que pueda añadir la cabecera de CORS en las peticiones.

3. Se añade las URLS donde va a recibir las peticiones. Se puede hacer para todas las URLs, mirar la documentación del paquete para más información, pero en mi caso lo voy acotar al servidor de React NextJS:

```tpl
CORS_ALLOWED_ORIGINS = [
    "http://localhost:3000"
]
```

