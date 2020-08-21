---
title: Autentificación por sesión
description: Autentificación por sesión
---

# Introducción

A través de *Django* se puede autentificar a los servicios de varias maneras. No es el objetivo de explicarlos aquí, pero el que he configurado es el de *Session*. E

Este tipo de autentificación hay un primer servicio que se le pasa el usuario y password y si es valido te genera una *cookie* de sesión que en los servicios siguientes no sea necesario pasar las credencilas. Se puede indicar el tiempo de expiración de la sessión para que de manera automática haga el logout.

Como la documentación de *Django* y *Django Rest FrameWork* es muy teoríca y poco práctica, al final gracias a un ejemplo he conseguido hacerlo funcionar. Por ello voy a poner aquí los pasos.

# Configuración

Hay que ir al archivo *settings.py* que esta dentro de las dos carpetas del mismo nombre que se genera el inicio del todo. Cualquier duda recomiendo al [índice](/docs/python/framework/django/) de *Django* para ver los primeros pasos.

En este archivo haremos las siguientes tareas.

1. Se añadira el tipo de autentificación en la variable *REST_FRAMEWORK*, bueno esta variable sirve para muchas cosas entre ellas la autentificación:

```tpl
REST_FRAMEWORK = {
    # Modo de autentificación global. Será por sesión
    'DEFAULT_AUTHENTICATION_CLASSES': [
        'rest_framework.authentication.SessionAuthentication'
    ],
    # Acceso a los servicios solo si están autentificados.
    'DEFAULT_PERMISSION_CLASSES': (
        'rest_framework.permissions.IsAuthenticated',
    )
}
```

*A modo informativo si no queremos que se haga ningún tipo de autentificación solo habría que poner:*
```tpl
REST_FRAMEWORK = {
 'DEFAULT_AUTHENTICATION_CLASSES': [],
 'DEFAULT_PERMISSION_CLASSES': [],
 'UNAUTHENTICATED_USER': None
}
```
Al final del archivo he puesto las siguiente variables:
```tpl
# Evita que se puede acceder a la cookie mediante javascript
# Más info: https://docs.djangoproject.com/en/dev/ref/settings/#session-cookie-httponly
SESSION_COOKIE_HTTPONLY = True

# Cuando se cierra el navegador la sesión caduca
SESSION_EXPIRE_AT_BROWSER_CLOSE = True

# Tiempo de validez de la session. Son en segundos. Le he puesto 60 minutos.
SESSION_COOKIE_AGE = 60*60
```
Los comentarios son autosuficientes para entender para que se usan.

# Serializadores

El serializador, o el controlador como yo lo llamo, es el que nos va gestionar si el usuario introducido existe. Yo en este caso he creado a nivel de proyecto un serializador porque al final el login/logout afecta a todos los servicios de toda las aplicaciones del proyecto.

Como el fichero *serializers.py* no existia lo he creado en la caperta donde tenemos el *settings.py* poniendo el siguiente código:

```tpl
from django.contrib.auth import authenticate
from rest_framework import serializers

"""
Clase que se encarga de validar que el usuario y password existen
"""


class LoginSerializer(serializers.Serializer):
    user = serializers.CharField()
    password = serializers.CharField()

    def validate(self, attrs):
        user = authenticate(
            username=attrs['user'], password=attrs['password'])

        if not user:
            raise serializers.ValidationError('Incorrect user or password.')

        if not user.is_active:
            raise serializers.ValidationError('User is disabled.')

        return {'user': user}
```

Lo bueno de los serializers en *Django Rest Framework* es que pueden detener su propio modelo sin tener que declararlo en base de datos.

# Vistas

Las vistas es el endpoint donde accede el servicio. De nuevo, he creado el fichero *views.py* en la misma carpeta que el serializer.

Las vistas usadas son las genéricas las que son *APIView*, el método aparte que sale en el ejemplo que he encontrado, es que hay que declararla el método HTTP que quieres usar. Si llamas al servicio con un método *POST* pero solo tienes definido el *GET* te va a dar un error que el método usado no es válido. 

Si se usará las vistas *ViewSet* no se puede hacer porque automimplementan ellas mismas todos los métodos.

El código es el siguiente:

```tpl
from django.contrib.auth import login, logout
from rest_framework.views import APIView
from rest_framework import permissions
from rest_framework import response
from rest_framework import status
from rest_framework import authentication
from . import serializers

"""
Solo se usa para el login y logout y desactivan la validación Csrf para poder hacer generar
la sesión o cancelarla.
"""


class CsrfExemptSessionAuthentication(authentication.SessionAuthentication):
    def enforce_csrf(self, request):
        return


"""
Se encarga del login, es el que hará la cookie de sesión.
Se usa el tipo de visto APIView, porque es la que se tiene que definir cada método para que funcione.
Ejemplo, tanto el login como logout cualquier llamada que no sea post devolverá un método no permitido.
"""


class LoginView(APIView):
    permission_classes = (permissions.AllowAny,)
    authentication_classes = (CsrfExemptSessionAuthentication,)

    def post(self, request):
        serializer = serializers.LoginSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        user = serializer.validated_data['user']
        login(request, user)
        return response.Response(status=status.HTTP_200_OK)


class LogoutView(APIView):    

    def get(self, request):
        logout(request)
        return response.Response()

```

En la clase de *LoginView* hay varias cosas interesantes:

1. *permission_classes* indica que o tiene permisos o no tiene. Porque hay permisos que indica si no tiene permisos solo puedes acceder a solo lectura. Aquí nada.
2. *authentication_classes* se le pasa una clase para que no valide el *Csrf token* que es obligatorio la autentificación por sesión. Este token se generara en la sentencia *login*

En la clase *LogoutView* se habilitado el método HTTP *GET* porque no es necesario pasar nada en el body para hacerlo el proceso.

# URLs

A nivel de proyecto hay que añadir el acceso a las dos vistas creadas. Para ello hay que modificar el archivo *urls.py*, que este se genera al crear el proyecto, y se añaden las siguientes líneas:
```tpl
urlpatterns = [
    path('admin/', admin.site.urls),
    path(r'login', views.LoginView.as_view(), name='login'),
    path(r'logout', views.LogoutView.as_view(), name='logout')
]
```

El path de *admin* ya viene de serie y se añade las dos vistas de login y logout.

# Funcionamiento

Las pruebas se han hecho con el programa *POSTMAN*. Para hacer el login és con la siguiente url: [http://localhost:8000/login](http://localhost:8000/login)

Al llamarlo a nivel de cabecera veremos las dos cookies que genera:

![Cookies sesión](/images/python/framework/django/cookies_session.png)

Para las siguiente llamadas hay que tener en cuenta que si son de tipo *POST* habrá que pasar informar en la cabecera de la llamada la variable *X-CSRFToken* con el valor devuelto en la cookie que tiene en el campo el literal *csrftoken=*, hay que coger todo la cadena de número hasta el *;*, sin incluirlo. Tal que así:

![Pasando el X-CSRFToken](/images/python/framework/django/valor_x_csrftoken_header.png)

Para los métodos *GET* no es necesario.