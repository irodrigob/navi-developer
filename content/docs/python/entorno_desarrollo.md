---
title: Entorno de desarrollo
weight: 20
---

# Introducción

Aquí hasta que no tenga más conocimientos voy ir anotando cosas que me sirven para preparar el entorno para desarrollo

# Python

El interprete del lenguaje hay que descargarlo de la página [oficial](https://www.python.org/).

# IDE de desarrollo

Uso el VS Code que tiene una ventaja cuando creas un programa con extensión ".py" (extensión que indica que es un lenguaje Python) te descarga el plugin para poder ejecutar directamente desde el VSCode.

Aunque en la [documentación de VS Code](https://code.visualstudio.com/docs/python/python-tutorial) explica como instalar los plugins necesarios.

## Jupyter Notebooks

Hay muchos cursos donde explican en lenguaje con Jupyter Notebooks. Esto es un proyecto opensource que permite ejecutar código Python como si fuese un lenguaje estructurado.  La ayuda oficial de como usarlo dentro del VS Code es [esta](https://code.visualstudio.com/docs/python/jupyter-support)


Para utilizarlo hay que pulsar *CTRL+SHIFT+P* para activar la ventana de comandos y escribir lo *Create Blank*:

![Ejecutar Jupyter Notebook](/images/python/entorno_desarrollo/ejecutar_jupyter_notebook.png)

Aparece el editor y ya lo podremos usar:

![Jupyter editor](/images/python/entorno_desarrollo/jupyter_editor.png)

Para probar lo que vamos escribiendo hay que poner en marcha el servidor de *Jupyter notebook*. Los pasos que he hecho han sido ir de nuevo la ventana de comandos y escribir *jupyter server*:

![Ejecutar servidor Jupyter](/images/python/entorno_desarrollo/jupyter_ejecutar_servidor.png)

Ha preguntado si el servidor sería local o remote, he escogido remoto:

![Tipo de servidor](/images/python/entorno_desarrollo/jupyter_servidor_conectar.png)

Luego ha preguntado que interprete de Python:

![Selector interprete](/images/python/entorno_desarrollo/jupyter_servidor_selector_interprete.png)

 y a continuación ha comenzado hacer una instalación en local.

*NOTA: Como tengo instalado de antemano Anaconda, y al tener dos ambientes configurados que a la postre son como tener dos interpretes de Python independientes,  me ha salido la ventana a escoger el interprete: el oficial de Python y los datos de Anaconda.*

El interprete puede ser cambiando abriendo la ventana de comandos:

![Selector interprete](/images/python/entorno_desarrollo/jupyter_servidor_cambiar_interprete.png)

Una vez instalado todo en la parte superior derecha del VS Code se verá el servidor activo:

![Servidor funcionando](/images/python/entorno_desarrollo/jupyter_servidor_funcionando.png)

Cuando volvamos abrir de nuevo el VSCode y abrir un editor del Jupyter Notebook el servidor arrancará automáticamente.


# Anaconda

[Anaconda](https://anaconda.org/) es un gestor de paquetes para poder utilizar los productos de Machine Learning como TensorFlow. 

Yo me he instalado la versión *64-Bit Grapical Installer*. 

Una vez instalado lo abriremos el menú de inicio de Windows y escribiremos *Anaconda*:

![Ejcutar Anaconda](/images/python/entorno_desarrollo/anaconda_menu_aplicaciones.png)

Una vez arrancado sale la siguiente pantalla:

![Anaconda navigator](/images/python/entorno_desarrollo/anaconda_navigator.png)

En la imagen se ve que estoy en un ambiente llamado *test* que he creado para hacer pruebas. Los ambientes, creo que se permiten hasta cinco, permites tener librerias distintas según el tipo de proyecto que se use.

Desde este menu lanzo el VS Code. Cuando lo lance desde esta opción se me instalo, eso creo, la extensión *Anaconda Extension Pack*:

![Anaconda Extension Pack](/images/python/entorno_desarrollo/anaconda_extension_pack.png)

Esta extensión usará la configuración de paquetes o librerias que tengas instalada en el ambiente que se tenga seleccionado.

Pero toda la parte de uso Anaconda lo iré poniendo en un artículo aparte.

