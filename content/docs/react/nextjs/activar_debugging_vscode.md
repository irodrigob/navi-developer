---
title: Activar debugging de Next JS en VS Code
description: Activar debugging de Next JS en VS Code
---

# Introducción

Es explicar los pasos para poder activar el debugging mediate VS Code.

# Primer paso instalar el paquete de variables de entorno

Hay que instalar el siguiente paquete:

```tpl
npm install -g win-node-env
```

Este paquete habilita poder usar variables de entorno en los scripts NPM en **Windows**. Para otros sistemas operativos esta este otro:
```tpl
npm install -g node-env
```

# Modificar archivo package.json

En el raíz de nuestro proyecto tendremos el archivo *package.json* donde el nodo de *scripts* tendremos lo siguiente:

```tpl
  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint"
  },
```

Pues en el apartado *"dev"* tendremos que cambiar y poner esto:

```tpl
 "dev": "NODE_OPTIONS='--inspect' next dev",
```

Ahora si ejecutamos el comando "npm run dev" en log nos saldrá algo parecido a esto:

```
Debugger listening on ws://127.0.0.1:9229/e2e31569-6b50-43e2-9ac5-d2c49da48306
For help, see: https://nodejs.org/en/docs/inspector
ready - started server on 0.0.0.0:3000, url: http://localhost:3000
```

Esto significa que el modo debuggin esta activo.

# Crear fichero *launch.json*

De nuevo en la raíz de nuestro proyecto tenemos que crear la carpeta *.vscode* y dentro crear el archivo *launch.json* con el siguiente contenido:

```tpl
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "attach",
            "name": "Launch Program",
            "skipFiles": [
                "<node_internals>/**"
            ],
            "port": 9229
        }
    ]
}
```

Esto hará que activará el debuggin por el puerto 9229 que es que usa NextJS. Ahora hay que abrir este archivo en VSCode y pulsar F5, o por el menú "Ejecutar->Iniciar depuración" y se nos abrirá la ventana de *CONSOLA DE DEPURACIÓN* donde aparecerá algo parecido a esto:

```
ready - started server on 0.0.0.0:3000, url: http://localhost:3000
info  - Loaded env from D:\github\nextjs-graphql-sample\.env.local
event - compiled client and server successfully in 1062 ms (171 modules)
```
Y en la propia consola donde hemos ejecuta el *npm run dev* no saldrá este este:

```
Debugger attached.
```

Con esto los breakpoints que pongamos en el código se nos parará cuando estemos probando la aplicación.




