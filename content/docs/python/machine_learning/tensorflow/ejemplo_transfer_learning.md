---
title: Ejemplo transfer learning
description: Transfer learning
---

# Introducción

Ejemplo extraído del video [Importar modelo para clasificar imágenes](https://www.youtube.com/watch?v=09_gmAeHIW0).

Y el modelo de datos de entrenamiento se puede descargar en el [repositorio del ejemplo de Alex Puig](https://github.com/puigalex/AMP-Tech/blob/master/CNN%20desde%20cero/TransferLearning(VGG16).ipynb).

El transfer learning o transferencia de aprendizaje es una técnica en la cual incorporamos un modelo ya pre-entrenado para utilizarlo para que clasifique lo que nosotros queramos.

Estos modelos son más complejos a nivel de configuración de redes convolucionales como el número de datos usados para entrenarlos. La gracia de usarlos es porque la configurad de la red detecta formas, sombras, cambios de contrastes, etc. Es decir, todo lo necesario para detectar una imagen. Estos modelos para reentranalos haría falta mucha fuerza computacional que uno no tiene en casa.

Keras tiene [modelos ya preentrenados](https://keras.io/api/applications/) que se pueden utilizar en nuestras propias clasificaciones. En el ejemplo del video se usará el modelo [VGG16](https://keras.io/api/applications/vgg/#vgg16-function) que contiene clasificación de mil imagenes distintas.

En el ejemplo del video se base en el [clasificador de imagenes](/docs/python/machine_learning/tensorflow/clasificador_imagenes_ejemplo.md) por lo que las explicaciones que hace cada parte del código esta explicado en la página enlazada. 

Unas de las cosas que ha mejorado al usar la red VGG16 es que la precisión al clasificar las imagenes ha mejorado muchísimo. Salvo los gorilas que al haber pocas imagenes la tasa de acierto sigo siendo muy baja.

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas. Pero para ver que tal ha mejorado la precisión hay que ejecutar el código de *predict.py* del clasificador de imagenes.

** NOTA IMPORTANTE: El algoritmo esta entrenado para imagenes 224x224 pixeles. Por lo tanto el codigo utilizado en el clasificador de imagenes hay que cambiarlo para adaptarlo a dicho tamaño**

# Código

[Código fuente descargable](/docs/python/machine_learning/tensor_flow/tf_ejemplo_transfer_learning.ipynb) 

```tpl
import sys
import os
import tensorflow as tf
import math
```
```tpl
# VGG16 es un modelo preentrenado que contiene multitud de imagenes.
# Este modelo preentrado es uno de los muchos que tiene Keras
vgg = tf.keras.applications.vgg16.VGG16()
```
```tpl
# Resumen de las capas que tiene la red neuronal VGG16
vgg.summary()
```
```tpl
# Creación de la red convolucional
cnn = tf.keras.Sequential()

# Se añaden las capas que se ha visto antes a nuestra propia red
for capa in vgg.layers:
    cnn.add(capa)
```
```tpl
# Resumen con las capas de nuestra propia red. Debe coincidir con el resumen
# realizado en la red VGG16
cnn.summary()
```
```tpl
# Con esto se elimina la capa de predicción. El pop elimina la ultima capa
# del modelo. La ultima capa era la de "predictions" que tiene 1000 parámetros, tipos de imagenes,
# que pued clasificar. Como queremos usarlo para el ejemplo de gatos, gorilas y perros, se quita dicha
# capa para añadir una propia.
cnn.pop()
```
```tpl
# Las capas que provienen del VGG16 no queremos entrenarlas porque ya lo han sido
# en el pasado. Por lo tanto, indicamos que no es necesario entrenarlas.
for layer in cnn.layers:
    layer.trainable = False
```
```tpl
# Añadimos una capa de predicción de softmax que tendrá 3 neuronas. Ya que se usará el ejemplo
# de gatos, gorilas y perros.
cnn.add(tf.keras.layers.Dense(3,activation='softmax'))
```
```tpl
# Esta función encapsula lo que hemos hecho en el código anterior paso a paso.
def modelo():
    vgg=tf.keras.applications.vgg16.VGG16()
    cnn=tf.keras.Sequential()
    for capa in vgg.layers:
        cnn.add(capa)
    cnn.pop()
    for layer in cnn.layers:
        layer.trainable = False
    cnn.add(tf.keras.layers.Dense(3,activation='softmax'))

    return cnn
```
```tpl
# Se limpia todas las variables, estado, etc. de sesión que hemos abierto de keras
tf.keras.backend.clear_session()
```
```tpl
# Trozo de código del clasificador de imagenes pero aquí no se indicarán las capas convolucionales
data_entrenamiento = "K:/github/python_test/tensorflow/clasificador_imagenes/data/entrenamiento/"
data_validacion = "K:/github/python_test/tensorflow/clasificador_imagenes/data/validacion/"

# Parametros de la red neuronal
# Número de veces que se iteran los datos en el entrenamiento
epocas = 20
# Tamaño de las imagenes. Este es el tamaño que espera la red VGG16, en la documentación
# oficial lo indica.
longitud, altura = 224, 224
# Numero de imagenes que se procesan en cada paso
batch_size = 32
# Al final de cada época se harán 300 pasos con los datos de validación
# para ir viendo como de bien va aprendiendo
validation_steps = 32
# Número de convoluciones o de capas(profundidad) que tendra la iamagen
# Primera convolucion serán 32
filtrosConv1 = 32
# Segunda convolucion serán 64
filtrosConv2 = 64
# Anchura y altura que va a procesar en cada convolucion.
tamano_filtro1 = (3, 3)
tamano_filtro2 = (2, 2)
# Tamaño que se va usar en el maxpooling
tamano_pool = (2, 2)
# Número de clases que hay en el set de datos: gatos, perros y gorilas
clases = 3
# Es el learning rate. Es decir, cuanto de grandes van a ser los ajustes en la red
# para ajustarse para buscar una solución óptima.
lr = 0.0004

# Como se van a transformar las imagenes para poderlas pasar al procesao de entrenamiento
entrenamiento_datagen = tf.keras.preprocessing.image.ImageDataGenerator(
    rescale=1. / 255,
    shear_range=0.2,
    zoom_range=0.2,
    horizontal_flip=True)

# Lo mismo pero para los datos de test
test_datagen = tf.keras.preprocessing.image.ImageDataGenerator(
    rescale=1. / 255)    

# Proceso de lectura y transformación de los datos para los datos de entrenamiento
entrenamiento_generador = entrenamiento_datagen.flow_from_directory(
    data_entrenamiento,
    target_size=(altura, longitud),
    batch_size=batch_size,
    class_mode='categorical')

# Lo mismo para los datos de test
validacion_generador = test_datagen.flow_from_directory(
    data_validacion,
    target_size=(altura, longitud),
    batch_size=batch_size,
    class_mode='categorical')    
# Se llama a la función que devolverá el modelo adaptado basandonas en la red BGG16
cnn = modelo()

# Se indica como aprenderá el algoritmo
cnn.compile(loss='categorical_crossentropy',
            optimizer=tf.keras.optimizers.Adam(lr=lr),
            metrics=['accuracy'])
# Calculo de pasos por epoca
pasos = math.ceil( ( len(entrenamiento_generador.filenames) / batch_size ) )    

cnn.fit(
    entrenamiento_generador,
    steps_per_epoch=pasos,
    epochs=epocas,
    validation_data=validacion_generador,
    validation_steps=validation_steps )

# Directorio donde se guardará el modelo
target_dir = 'K:/github/python_test/tensorflow/clasificador_imagenes/modelo'
if not os.path.exists(target_dir):
 os.mkdir(target_dir)

# Grabación del modelo
cnn.save('K:/github/python_test/tensorflow/clasificador_imagenes/modelo/modelo.h5')

# Grabación de los pesos de cada una de las capas
cnn.save_weights('K:/github/python_test/tensorflow/clasificador_imagenes/modelo/pesos.h5')   
```
