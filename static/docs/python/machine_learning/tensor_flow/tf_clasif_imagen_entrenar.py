import sys
import os
# Ajusta el nivel de log que se muestra en consola. Son logs que son informativos
# y generán las propias librerías. Los valores posibles son:
# Level | Level for Humans | Level Description                  
# -------|------------------|------------------------------------ 
#  0     | DEBUG            | [Default] Print all messages       
#  1     | INFO             | Filter out INFO messages           
#  2     | WARNING          | Filter out INFO & WARNING messages 
#  3     | ERROR            | Filter out all messages      
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  

import tensorflow as tf
import math

# Código que soluciona el error: Could not create cudnn handle: CUDNN_STATUS_INTERNAL_ERROR
# debido a temas de memoria.
physical_devices = tf.config.experimental.list_physical_devices('GPU')
assert len(physical_devices) > 0, "Not enough GPU hardware devices available"
config = tf.config.experimental.set_memory_growth(physical_devices[0], True)

# Path donde están las imagenes de entrenamiento y test
data_entrenamiento = "K:/github/python_test/tensorflow/clasificador_imagenes/data/entrenamiento/"
#data_entrenamiento = "./data/entrenamiento/"
data_validacion = "K:/github/python_test/tensorflow/clasificador_imagenes/data/validacion/"
#data_validacion = "./data/validacion/"

# Parametros de la red neuronal
# Número de veces que se iteran los datos en el entrenamiento
epocas = 20
# Tamaño de las imagenes
longitud, altura = 100, 100
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
lr = 0.0005

# Preprocesamiento de la imagen
# Paso necesario para poder enviar las imagenes a la red

# Creamos un generador que se indicará como tiene que procesarlas
# El rescale= 1./255 -> Cada uno de los pixeles tiene un rango de 0 a 255 (escale RGB).
# Con esto estos pixeles se convierte de 0 a 1 para que sea más eficiente el algoritmo.
# El shear_range=0.2 -> Va a inclinar de manera aleatoria la imagen para que el algoritmo no se piense
# que la imagen va a estar recta.
# El zomm_range=0.2 -> Va hacer zoom de manera aleatoria para que el algoritmo aprenda a que
# la imagen no tiene porque estar completa
# horizontal_flip=True -> Va a invertir de manera aleatoria la imagen. Para que el algoritmo aprenda direccionabilidad
entrenamiento_datagen = tf.keras.preprocessing.image.ImageDataGenerator(
    rescale=1. / 255,
    shear_range=0.2,
    zoom_range=0.2,
    horizontal_flip=True)

# Para el set de datos solo se hace el rescalado. Ya que se quiere comparar imagenes reales.
test_datagen = tf.keras.preprocessing.image.ImageDataGenerator(
    rescale=1. / 255)

# Se genera la variable donde se le indicará donde están los datos que tiene que leer.
# Se le pasa:
# - Ruta donde se leerán las imagenes, incluidas sus subdirectorios
# - Altura y longitud de la imagen
# - Tamaño del batch
# - El tipo de clasificación, que indicará que las etiquetas son los directorios donde
#   están las imagenes
entrenamiento_generador = entrenamiento_datagen.flow_from_directory(
    data_entrenamiento,
    target_size=(altura, longitud),
    batch_size=batch_size,
    class_mode='categorical')

validacion_generador = test_datagen.flow_from_directory(
    data_validacion,
    target_size=(altura, longitud),
    batch_size=batch_size,
    class_mode='categorical')

# Las etiquetas las genera de manera ordenada. Cuando se haga la predicción hay que saber la relación entre número que y 
# devolverá y su etiqueta correspondiente. Esto se puede saber ordenando la carpeta de window y viendo el orden. O bien
# hacer lo siguiente e imprimirlo en pantalla:
label_map = (entrenamiento_generador.class_indices)
print(label_map)

# Creación de la red convolucional
cnn = tf.keras.Sequential()

# Primera capa de la red que será la de la convolución. Donde se le pasa:
# - Filtro que hemos configurado
# - Tamaño del filtro que hemos configurado
# - padding="same" es como se comporta el filtro en las esquinas
# - input_shape es el tamaño de la imagen que se le va a pasar a la capa.
# el 3 es por las capas RGB de la imagen
# - activation es la función de activación
cnn.add(tf.keras.layers.Conv2D(filtrosConv1, 
                               tamano_filtro1,
                               padding="same", 
                               input_shape=(longitud, altura, 3), 
                               activation='relu'))

# Segunda capa para realizar el maxpooling donde se le pasa el tamaño definido
# en la variable
cnn.add(tf.keras.layers.MaxPool2D(pool_size=tamano_pool))

# Tercera capa que será la segunda convolución.
# En esta capa no se le pone en input_shape porque esto se hace en la primera porque recibe
# las imagenes tal cual. En esta segunda convolución el tamaño de la imagen ya ha sido
# ajustada en las capas anteriores. 
cnn.add(tf.keras.layers.Conv2D(filtrosConv2, 
                               tamano_filtro2,
                               padding="same"))

# Cuarta capa que será el pooling de la convolución anterior
cnn.add(tf.keras.layers.MaxPool2D(pool_size=tamano_pool))

# Ahora se realiza la configuración de la clasificación

# Cuando llegue aquí la imagen será muy pequeña pero con muchas capas. Lo que hace flatten
# es convertirlo a una sola dimensión. Es decir, la aplana
cnn.add(tf.keras.layers.Flatten())
# Esto lo que hace es crear una capa "normal" donde donde va estar conectada con la capa
# anterior que tiene la información aplanada.
# Esta capa va a tener 256 neuronas y la áctivación va ser la "relu"
cnn.add(tf.keras.layers.Dense(256, activation='relu'))
# Esto lo que hace es apagar el 50% de las neuronas, aleatoriamente, de la capa anterior en cada paso. Esto
# se hace va evitar los sobreajustes. Si todas las neuronas están activas en todos los pasos
# es posible que la red aprende un camino específico para clasificar. Haciendo esto, va aprender caminos
# alternativos para la clasificación, evitando el sobreajuste.
cnn.add(tf.keras.layers.Dropout(0.5))
# La última capa de la red para a tener 3 neuronas, lo indicado en la variable clases(perro, gato y ave).
# Esta función es la que va a decir el % de probabilidad a que clase pertenece.
cnn.add(tf.keras.layers.Dense(clases, activation='softmax'))

# Este es el paso de configuración del aprendizaje del modelo
# El parámetro "loss" es la función de perdida, es decir, es lo que indica que tan bien esta aprendiendo
# En el parámetro "optimizer" es el algoritmo que se usa para el calculo de peso y el gradiente descendiente. 
# El algoritmo será el "Adam" y su learning rate será el indicado por la variable global
# El parámetro "metrics" que será el cual vamos a estar optimizando será "accuracy" que indica el % de que tal
# esta aprendiendo el algoritmo.
cnn.compile(loss='categorical_crossentropy',
            optimizer=tf.keras.optimizers.Adam(lr=lr),
            metrics=['accuracy'])

# Los pasos que se hará por época depende del tamaño de datos y el valor del batch_size. Ya que si se pone un dato 
# menor no se procesarán todos los datos y un dato superior dará el error:
# "Your input ran out of data; interrupting training. Make sure that your dataset or generator can generate at least `steps_per_epoch * epochs` batches"
# por eso, hay que aplicar la siguente formula para que los pasos sea los justos para procesar toda la información
pasos = math.ceil( ( len(entrenamiento_generador.filenames) / batch_size ) )

# Se entrena el algoritmo pasandole:
# Datos de entrenamiento
# Pasos por cada epocas. Es la operación de dividir el tamaño total de datos / 
# Número de epocas
# Datos de validacion
# Número de pasos con los datos de validación
# Esto lo que va hacer es en cada epoca va hacer mil iteracciones para entrenar el algoritmo. Cuando termine
# esa epoca va hacer doscientos pasos de validación para como de vien va el entrenamiento. Y luego volverá a
# procesar la siguiente epoca.
cnn.fit(
    entrenamiento_generador,
    steps_per_epoch=pasos,
    epochs=epocas,
    validation_data=validacion_generador,
    validation_steps=validation_steps )
# El resultado del entrenamiento se guardará en el directorio "modelo", que se creará si no existe previamente
#target_dir = './modelo/'
target_dir = 'K:/github/python_test/tensorflow/clasificador_imagenes/modelo'
if not os.path.exists(target_dir):
 os.mkdir(target_dir)

# Grabación del modelo
#cnn.save('./modelo/modelo.h5')
cnn.save('K:/github/python_test/tensorflow/clasificador_imagenes/modelo/modelo.h5')
# Grabación de los pesos de cada una de las capas
#cnn.save_weights('./modelo/pesos.h5')
cnn.save_weights('K:/github/python_test/tensorflow/clasificador_imagenes/modelo/pesos.h5')