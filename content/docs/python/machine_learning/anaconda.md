---
title: Anaconda
description: Anaconda
---

# Introducción

Tal como se ha explicado en la página de [entorno de desarrollo](/docs/python/entorno_desarrollo.md), Anaconda permite la instalación de paquetes para usar el machine learning en Python.

# Entonros

Los entornos, creo que hasta tres entornos se pueden usar, permiten tener instalaciones separadas de paquetes según lo que se quiera hacer.

Para hacer pruebas me he creado un ambiente llamado *test* que lo he creado con el *Anaconda navigator* yendo a la pestaña de *Enviroments*:

![Entornos](/images/python/machine_learning/anaconda/entornos_anaconda_navigator.png)

Y luego pulsando el botón de *Create* que es la parte inferior de la lista de ambientes.


![boton crear](/images/python/machine_learning/anaconda/boton_crear_entorno.png)

Te pide los datos del entorno :

![Ventana crear el entorno](/images/python/machine_learning/anaconda/ventana_crear_entorno.png)

Y al pulsar el botón de *Create* comenzar a instalar los paquetes necesarios y algunas librerias.

# Activar entorno

Por defecto esta el entorno *base* activado. Para cambiar de entorno se puede hacer de dos maneras. 

1. Es mediante el Anaconda Navigator, ir a la pestaña de *Environments* y seleccionar el entorno:

![Seleccionar entorno](/images/python/machine_learning/anaconda/seleccionar_entorno.png)

Tarda unos segundos en activarse.

2. A través de la consola. Abrir el menu de inicio de Windows, escribir *Anaconda* y seleccionar el modo consola:

![Consola de anaconda](/images/python/machine_learning/anaconda/consola_anaconda.png)

Y escribir:

```tpl
conda activate <nombre entorno>
```

Como en la imagen:

![](/images/python/machine_learning/anaconda/activar_entorno_consola.png)

A la derecha de la consola nos pone el entorno por defecto. Al escribir y pulsar *Enter*

![Activar entorno 2](/images/python/machine_learning/anaconda/activar_entorno_consola2.png)

Y veremos que a la derecha sale el entorno seleccionado.

# Instalar libreria

## Vía consola
La libería de TensorFlow no viene instalado. Vamos a instalar en el entorno de *test* y vía consola.

```tpl
conda installa tensorflow
```

Sale información sobre lo que se va a instalr_

![Instalar TensorFlow](/images/python/machine_learning/anaconda/instalar_paquete_tensorflow.png)

Importante en la imagen se ve que va instalar TensorFlow en el entorno *test*, el activo. Si tenemos otro entorno que lo queramos instalar hay que activar dicho entorno e instalarlo.

Se le pulsa que *y* y comenzará a realizar la instalación. Nos irá diciendo del progreso de instalación pero no tarda mucho. Con eso ya se puede usar el TensorFlow.

## Vía entorno gráfico

Vamos a instalar la librería *Scikit lear(sklearn)* para ello primero hay activar el entorno donde la queremos, ir a la pestaña de *Environments*, seleccionar los paquetes no instaldos y filtrar por *scikit*:

![Instalacion mediante entorno gráfico](/images/python/machine_learning/anaconda/instalacion_libreria_entorno_grafico.png)

En la imagen ya se ha marcado el paquete que nos interesa y pulsar el botón de *Apply* situado en la parte inferior derecha.
**NOTA: Al marca los tres paquetes de *scifi* no hay manera que se instalen por eso solo marco uno**

![Iniciar instalación](/images/python/machine_learning/anaconda/iniciar_instalacion_paquete_entorno_grafico.png)

Si hay dependencias con otros paquetes nos pedirá que confirmemos los otros paquetes:

![Ventana dependencias](/images/python/machine_learning/anaconda/instalacion_grafica_ventana_dependencias.png)

Se pulsar el botón *Apply* para iniciar la instalación. Una vez instalado el paquete desaparecerá de la lista de paquetes no instalados.

También he instalado el paquete *scikit-image*. El otro paquete asociado *scikit-rf* da error al instalarlo.