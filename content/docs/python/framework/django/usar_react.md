---
title: Como utilizar React como frontend
description: Como utilizar React como frontend
---

# Introducción

Lo primero es tener [nwb](/docs/react/nwb/) instalado en la maquina.

Según he léido en articulos hay como tres posiblidad de hacerlo:

1. Tener la aplicación React y django por separado pero habría que usar [JWT](https://jwt.io/) para poderse comunicar. Aunque yo no tengo tan claro si uno tiene el montaje de sesión como hice en este [artículo](/docs/python/framework/django/autentificacion_session.md). Ya que el primer servicio que lanzas en React es para generar el token de sesión y luego lo vas usando para las distintas llamadas. Este indican que la configuración es complicada, y me lo creo con lo que me costo entender y hacer la de sesión
2. Tener React dentro de django. Esta ponen que la dificultad es media.
3. Usar componententes React en Django. Esta indican que la dificultad es fácil.

Yo de momento voy a optar por la solución 2. De esta manera espero ahorrarme tener dos servidores corriendo.

# React dentro de Django

Todos los pasos que he hecho ha sido dentro de la pestaña de terminal del VS Code con el proyecto abierto.

## Creación de la aplicación Rest

La aplicación de React hay que hacerla dentro del proyecto de Django mediante el siguiente comando:

```tpl
nwb react-app reactjs
```

En la consola veremos los directorios y archivos que se generán

## Ajustando el .gitignore

Este punto no lo he encontrado en ningún sitio. Pero si nos fijamos en el resultado de la consola el comando anterior crear el _.gitignore_ para los archivos que no se quieren subit a Git. El mótivo es que este comando crea la aplicación como su fuese standalone. Entonces lo que he hecho es añadir los archivos del archivo al _.gitignore_ de directorio principal del proyecto en VS Code. Quedando así:

```tpl
**/dist
**/node_modules
**npm-debug.log*
```

Le he puesto "\*\*" delante para que los ignore independietemente del nivel de directorio donde este. De esta manera me evito poner todo el path

## Configurando los templates en Django

Las plantillas en Django permite crear páginas Web y que se renderizen en las vistas. Lo que se quiere conseguir es que la plantilla arranque la aplicación React.

Para ello dentro del directorio del proyecto de Django se creará la carpeta _templates_ y a continuación desde el VS Code se creará el archivo _index.html_ con el el texto

```
Punto de acceso a React
```

Esto es para probar que la configuración inicial funciona.

Ahora hay que indicaré a Django donde están los templates. Para ello hay que modificar el archivo del proyecto Django llamado _settings.py_ y buscar la variable _TEMPLATES_ y ajustarla para que quede tal que así:

```tpl
TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [
            os.path.join(BASE_DIR, 'templates')
        ],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]
```

Lo único que se añade es esta línea _os.path.join(BASE_DIR, 'templates')_ ya que el resto ya viene informado al crear el proyecto.

En la misma carpeta que están la configuración del proyecto hay que modificar las URLs del proyecto en el archivo _urls.py_ añadiendo lo siguiente:

```tpl
from django.views.generic import TemplateView
path(r'', TemplateView.as_view(template_name="index.html")),
```

Esto indica que cuando se acceda al path inicial del servidor se redirigira al archivo _index.html_

Ahora hay que arrancar el servidor de Django y acceder a la URL inicial y si vemos el texto que se ha puesto en el fichero HTML es que la configuración es correcta.

## Enlanzar React con Django

El último paso es poder enlazar React con Django. Para ello se instalará a nivel local el plugin _[html-webpack-plugin](https://github.com/jantimon/html-webpack-plugin)_ y el _[html-webpack-harddisk-plugin](https://github.com/jantimon/html-webpack-harddisk-plugin)_. Esto permite hacer uso de [Webpack](https://webpack.js.org/) que permite encapsular todo el código en unos archivos que entienda Django.
El segun plugin amplia la funcionalidad del primero para que siempre escriba en disco.

Para instalarlo hay que ir al directorio de la aplicación React creada dentro del proyecto de Django y ejecutar el siguiente comando:

```tpl
npm install --save-dev html-webpack-plugin
npm install --save-dev html-webpack-harddisk-plugin
```

Dara unos avisos de dependencias que hay instalar manualmente pero no hay que hacer nada porque ya estan incluidas con _nwb_

Ahora hay que editar el fichero _nwb.config.js_ que esta en el directorio de la aplicación React y añadir lo siguiente:

```tpl
const HtmlWebpackHarddiskPlugin = require("html-webpack-harddisk-plugin");
const path = require("path");
const isPro = process.env.NODE_ENV === "production";

module.exports = {
  type: "react-app",

  webpack: {
    // dont forget delete src/index.html to use this config: mountid, title, favicon
    html: {
      mountId: "app",
      title: "OCR Invoices",
      // favicon: 'src/favicon.ico'
      //this setting is required for HtmlWebpackHarddiskPlugin to work
      alwaysWriteToDisk: true,
      filename: "index.html",
    },
    publicPath: isPro ? "/static/" : "http://localhost:3000/",
    extra: {
      plugins: [
        // this will copy an `index.html` for django to use
        new HtmlWebpackHarddiskPlugin({
          outputPath: path.resolve(__dirname + "/../", "templates"),
        }),
      ],
    },
    config: function (config) {
      if (!isPro) {
        config.entry = [
          "webpack-dev-server/client?http://localhost:3000",
          "webpack/hot/only-dev-server",
          "./src/index.js",
        ];
      }

      return config;
    },
  },
  devServer: {
    // allow django host, in case you use custom domain for django app
    allowedHosts: ["localhost"],
  },
};
```

## Arrancar

Para arrancar React a nivel de la carpeta de la aplicación de React hay que lanzar el siguiente comando:

```tpl
npm start
```

Esto arranca el servidor React en el puerto 3000. Y lo que va hacer es sobreescribir el fichero _index.html_ de la carpeta de _templates_ para que se vea lo que hay en el servidor de React.

Es decir. Que si accedemos a _http://localhost:8000/_ veremos lo mismo que si accedemos a _http://localhost:3000/_

** NOTA IMPORTANTE INSTALACIÓN **

Esto tal cual da un error porque el módulo _html-webpack-plugin_ requiere del _webpack_ instalado al mismo nivel que el plugin, es decir, a nivel de proyecto. Pero he hecho tantas instalaciones a nivel global, como local, que ahora no sé como me funciona.

## Paso para desplegar en producción

Esta explicado en este [artículo](https://tamhv.github.io/2018/05/14/Setup-django-with-react-using-nwb/) pero pongo aquí el texto por si el artículo se pierde cuando lo necesite:

```tpl
npm run build

All js files should be ready in reactjs/dist. Check templates/react.html to see the changes.

Now we need to tell django go to collectstatic

Edit settings.py

1
2
3
4
STATIC_ROOT = os.path.join(BASE_DIR, 'static_dist')
STATICFILES_DIRS = [
    os.path.join(BASE_DIR, 'reactjs/dist'),
]
Restart django app and check again http://127.0.0.1:8000/, react js is served by django static

commit dist or not! it doesn’t matter
We will want to fix reactjs/.gitignore if we want to commit dist, otherwise our CI server must do yarn build first, then collectstatic

Now we have an app with react at frontend, backend by django, next step is add django rest framework,…etc

Github: https://github.com/tamhv/djangoreact
```

**Nota: he cambiado el modo es que se producen los archivos para su distribución**
