---
title: Desarrollo con VS Code
description: Desarrollar aplicaciones con Visual Studio Code
---

# Objetivo

Para desarrollar en UI5 he probado tres editores: [WebStorm](https://www.jetbrains.com/webstorm/), [Sublime](https://www.sublimetext.com/) y [Visual Studio Code](https://code.visualstudio.com/). 

WebStorm lo use hace años y me fue tan bien que pague por la licencia. El tema de reconocer configurar, eso hace años, las librerias era con una extensión pero no esta la última versión.

Sublime lo empece a usar hace poco para mi último proyecto y fue el único que era capaz de navegar entre las distintas funcionaes de javascript que iba creando. Pero su entorno no me terminaba de encajar. Y la extensión de librerias de UI5 esta más desfasada.

Visual Studio Code. Es el que uso ahora, el único pero gordo es que no puedes navegar entre funcionens que esten entre distintos archivos. Esto hace que uses mucho los buscadores o tener buena memoria. Además, tiene extensiones para generar las vistas XML. El entorno lo veo más amigable y es el que me siento a gusto. Además, creo que es el que tiene más recorrido por todo su ecosistema de extensiones.

En este artículo explicaría sin mucho detalle, a menos de momento, como he configurado mi entorno para desarrollar en UI5 con VS Code.

# Preparar entorno en VS Code

## Prerequisitos

Hay que tener instalado el [Visual Studio Code](https://code.visualstudio.com/) y [Node.js](https://nodejs.org/en/).

## Paquetes del node.js

1. Mediante la consola de windows instalar el paquete "generator-easy-ui5" para simplificar la creación de proyectos nuevos, como vistas, etc.. Se instala con la sentencia:

```tpl
npm install -g yo generator-easy-ui5 
```

## Configuración del VS Code


1. Instalar la extensión [SAPUI5 Extension](https://marketplace.visualstudio.com/items?itemName=iljapostnovs.ui5plugin) de VS Code. Esta extensión te permite autocompletar código en vistas XML y controladores. Yo ahora mismo no lo tengo ni instalado porque he ido haciendo las cosas a base de ejemplos de las propias página de UI5 y su API. Si lo instalamos hay que modificar la configuración de las extensión para:

 * Cambiar el nombre de la carpeta donde esta el codigo fuente. Como se va usar el "easy-ui5" se cambia el valor "src" por "webapp".
 * Hay que indicarla versión de SAPUI5 que se va a usar, con la versión 1.73.1 funciona sin problemas

2. Instalar la extensión [XML Tools](https://marketplace.visualstudio.com/items?itemName=DotJoshJohnson.xml) para que te haga pretty pinter el as vistas XML. Si queremos los atributos en cada línea, y no todo en una línea que es la opción por defecto, hay que hay ir a la configuración de las extensión, es la que se llama *XML Tools Configuration*, y marcar el flag que se llama *Split attributes on Format*

## Preparar el proyecto

1. Abrir la consola de Windows e ir a la carpeta donde se alojara el proyecto. **IMPORTANTE: No es necesario crear la carpeta del proyecto***

2. Ahora ejecutar el comando:

```tpl
yo easy-ui5
```

Para hacer la configuración inicial del proyecto. Seguir las preguntas del asistente. Este paso sobrescribe aspectos de la configuración del paso 2. A día de hoy no tengo claro si se puede obviar el paso 2 ya que a lo mejor este paso crea el *package.json*.
Las opciones para mis proyectos personales han sido:

![yo easy-ui5](/images/sap/ui5/desarrollar_vscode/yo_easy_ui5.png)

En las preguntas tenemos la posibilidad de crear la carpeta donde estará el proyecto. Además instalará las dependencias necesarias.

**Cosas a tener en cuenta:**

* El nombre del *namespace* se concatena al nombre de la carpeta. Creo que en su momento yo puse un *namespace* pero le indique que no creará la carpeta.

3. Ahora abrimos el VS Code y se abre la carpeta del proyecto. Ahora en la parte inferior hay varias pestañas, ir a la de *Terminal* y pondremos:

```tpl
npm start
```

## Tareas opcionales

### Instalar servidor proxy 

Como en mi caso voy a llamar a servicios que están en un sistema SAP hay que instalar un proxy para evitar el cross domain. Para eso hay que hacer lo siguiente:

* Desde la consola de Windows ejecutar el siguiente comando: 
```tpl
npm install ui5-middleware-simpleproxy --save-dev
```
* Dentro de la carpeta del proyecto hay que modificar el archivo *ui5.yaml* y poner algo parecido(digo parecido porque cada uno tendrá un servidor distinto):

```tpl
Srver:  
server:
  customMiddleware:
  - name: ui5-middleware-simpleproxy
    afterMiddleware: compression
    mountPath: /sap/opu/odata/sap
    configuration:
      baseUri: "http://vhcalnplci.dummy.nodomain:8000/sap/opu/odata/sap"
```

Esto lo que hace que cualquier petición local a */sap/opu/odata/sap* se redirigia a la url indicada en *baseUri*. El nombre del servidor es el SAP pone a sus ABAP Trial Version.
* En el fichero *package.json" hacer lo siguiente:
 * Sección *devDependencies* modificar:
 ```tpl
"ui5-middleware-simpleproxy": "*"
```
Se pone el asterisco en la versión para que sea válido para cualquier versión del paquete.

 * Añadir la siguiente sección, yo la he puesto justo después de *devDependencies*:

 ```tpl
  "ui5": {
    "dependencies": [
      "ui5-middleware-simpleproxy"
    ]
  }
```



Esto arrancará el servidor web con la aplicación si todo esta bien arranca la aplicación template que se ha creado con el *yo easy-ui5*.

### Subir el proyecto a una BSP

Desde el VSCode es posible subir el proyecto a una BSP, en caso que las usemos para publicar nuestros aplicaciones. Para ello hay que instalar el paquete [ui5-task-nwabap-deployer](https://github.com/pfefferf/ui5-nwabap-deployer/tree/master/packages/ui5-task-nwabap-deployer) con el comando:

```tpl
npm install ui5-task-nwabap-deployer
```

Ahora en el fichero *ui5.yaml*, que esta en el raíz del proyecto, hay que añadir las siguientes líneaS:

```tpl
builder:
  customTasks:
  - name: ui5-task-nwabap-deployer
    afterTask: generateVersionInfo
    configuration: 
      resources:
        pattern: "**/*.*"
      connection:
        server: http://myserver:8000  
      authentication:
        user: myUser
        password: myPassword
      ui5:
        language: EN
        package: ZZ_UI5_REPO
        bspContainer: ZZ_UI5_TRACKED
        bspContainerText: UI5 Upload
        transportNo: DEVK900000
        calculateApplicationIndex: true  
```

Los datos a poner son los suficientes intuitivos y no es necesario explicarlos.

Ahora para deployarlo en la BSP tan solo hay que ejecutar el siguiente comando:

```tpl
npm run build:ui
```

Con esto sube la aplicación a la BSP indicada. Si no esta creada, la crea.



