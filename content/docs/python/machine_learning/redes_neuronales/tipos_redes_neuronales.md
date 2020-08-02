---
title: Tipos de redes redes neuronales
description: Tipos de redes redes neuronales
---

# Introducción

Ejemplo extraído de [Tipos de redes neuronales](https://www.youtube.com/watch?v=V5BYRPJThjE). Se pondrán imagens obtenidas del video para explicar mejor el funcionamiento tal como lo hace en el video.

En el video habla de los siguientes tipos:

* Profundas, o DNN.
* Convolucionales, o CNN.
* Recurrente, o RNN.

# Deep Neural Net(DNN)

Es una red versátil que permite procesas:

* Texto
* Imagenes pequeñas
* Datos númericos. Como el set de datos de Iris.

La estructura es la que más se ha visto en los ejemplos:

![Estructura DNN](/images/python/machine_learning/redes_neuronales/tp_redes_estructura_dnn.png)

En esta estructura se reciben los datos en la capa de entrada se procesan en la capa oculta y se el resultada se ve en la capa de salida. Cada neurona de cada capa esta interconectada con las neuronas de las siguientes capas. 

Este tipo de red se le llama profundas porque dentro de las capas ocultan hay más subcapas. 

El problema de este tipo de red es que debido a la multitud de conexiones que hay entre capas, hace que el cálculo sea muy pesado a medida que el tamaño de los datos de entrada sea más grande. Ejemplo una imagen de 300x300 pixeles equivale a 90.000 entradas. En el [ejemplo de Tensor Flow de reconomiento de números escritos a mano](/docs/python/machine_learning/tensorflow/reconocimiento_numeros_escrito_mano.md), donde se explica la versión que funciona, hay un ejemplo del número de parámetros que necesita la red para procesar los datos.

Para el este tipo de tareas, como el de procesar imagenes, se ha ido la red CNN.

# Convolutional Neural Net(CNN)

Esta red aunque también se uas para procesar textos su punto fuerte, y donde más se usa, es para el procesamiento de imagenes. La estructura de red es parecida a la anterior solo cambia como procesa las capas ocultas:

![Estructura red CNN](/images/python/machine_learning/redes_neuronales/tp_redes_estructura_cnn.png)

Las capas ocultas son las *convoluciones* y *maxpooling*. A medida que va procesando la imagenes en las distintas capas va reduciendo el tamaño de la imagen, *pooling*, y a través del *convoluciones* para detectando o identificando los valores más importantes. 
Cada capa que se va procesando va teniendo un nivel de abstracción más elaborado a medida que se acerca a la capa de salida.

Ejemplo, si se hace un clasificador de imagenes de coches. En la primera capa va identificar líneas(rectas, diagonales, etc..), en la siguiente capa va a encargarse de realizar figuras(cuadrados, circulos, rombos, etc.), en la siguiente capa va a identicar elementos más elaborados como llantas, puertas, etc.

# Recurrent Nerual Net(RNN)

Se usa para tipos de datos que suelen ser secuenciales. Ejemplo el precio de una acción, es un valor sencuencial cuyo valor depende de los valores que haya tenido en días pasados. 
Este tipo de red se usa mucho para texto ya que al final texto es secuencial. La estructura es la siguiente:

![Estructura RNN](/images/python/machine_learning/redes_neuronales/tp_redes_estructura_rnn.png)

Las capas ocultas también se llaman LSTM(Long Short Trem Memory/Memoria corta a largo plazo). Este tipo de capas reciben una información, hacen una predicción y la salida sirve para volver a alimentar la capa oculta. De esta manera la red sabe lo que sucedio antes. 

Ejemplo del video: El problema es *Hola cómo estas <nombre>*. A la capa inicial se le pasa el *hola*:

![RNN - Ejemplo 1](/images/python/machine_learning/redes_neuronales/tp_redes_estructura_rnn_ejemplo1.png)

El capa oculta predice que la siguiente palabra será *cómo*. Que vuelve a pasarsela como parámetro de entrada de nuevo a la capa oculta para la siguiente predicción. La capa oculta detecta que ahora me envías la palabra *cómo* pero antes me has pasado la palabra *hola*, por la tanto determina que la siguiente palabra tiene que ser *estas*

![RNN - Ejemplo 2](/images/python/machine_learning/redes_neuronales/tp_redes_estructura_rnn_ejemplo2.png)

De nuevo la salida de la capa vuelve a ser la entrada de la capa oculta. Con lo cual el sabrá que las palabras anteriores han sido *hola* y *como* y puede determinar que seá *<nombre>*


