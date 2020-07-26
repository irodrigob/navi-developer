---
title: Reconocimiento números escritos a mano
description: Reconocimiento números escritos a mano
---

# Introducción

Ejemplo extraído del video [Tensorflow: Cómo clasificar números escritos a mano](https://www.youtube.com/watch?v=EIXnx4BA1JY). 

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

**NOTA: El código del video esta basado en Tensor Flow 1.0. Pero el Tensor Flow que se ha instalado es la version 2.x, la llamaremos TF2.0 o TF20 . Por lo tanto hay muchas cosas que no son compatibles. En el código habrá partes 
las principales que se indique que no se usa en TF20, pero el resto de partes se pondrá en código TF20 para no poner demasiada basura en el código**

Este ejemplo según el video es el clásico *Hola mundo* de las redes neuronales. El problema se basa al que al algoritmo se le van a pasar uno números escritos a mano y el nos va a devolver cual es el número. En el video lo llamada el problema de [MNIST](https://en.wikipedia.org/wiki/MNIST_database). Pero en Wikipedia indica que es una base de datos de números escritos a mano que se usa para entrenar algoritmos.

En el ejemplo se va a crear un red neuronal que solo va a tener dos capas: 

1. Capa que recibe los pixeles de las imagenes
2. Capa de salida que será un vector de diez elementos donde nos dirá que número del 0 al 9 se corresponde la imagen introducida

**AVISO IMPORTANTE**

El paquete de set de datos de MINST no se incluye por defecto al instalar el paquete *Tensor Flow* vía *Anaconda*, ni siquiera aparece instalando el paquete *tensorflow-datasets*. La solución más sencilla es:

1. Descargarse, o clonar lo que se quiera, el repositorio de [Github](https://github.com/tensorflow/tensorflow) y copiar la carpeta "tutoriales
2. Descomprimir o navegar dentro del zip a la carpeta *tensorflow\examples*, estará la carpeta *tutorials*. Copiarla.
3. Pegar en el directorio del entorno de *Anaconda* donde ejecutaremos el ejemplo. En mi caso esta aquí: *D:\Users\ivan\anaconda3\envs\test\Lib\site-packages\tensorflow_core\examples*
4. Con eso ya no dará error al importar el set de datos.

 # Código

 [Código fuente descargable](/docs/python/machine_learning/tensor_flow/.ipynb) 

 ![Vector Softmax](/images/python/machine_learning/tensor_flow/num_mano_vector_softmax.png)