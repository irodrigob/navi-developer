import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  

import numpy as np
import tensorflow as tf

# Altura y longitud de la imagen que tiene que ser la misma que se ha usado para el entrenamiento
longitud, altura = 100, 100
# Ruta donde esta el modelo y los pesos
modelo = 'K:/github/python_test/tensorflow/clasificador_imagenes/modelo/modelo.h5'
pesos_modelo = 'K:/github/python_test/tensorflow/clasificador_imagenes/modelo/pesos.h5'

# Se carga el modelo keras
cnn = tf.keras.models.load_model(modelo)

# Se carga los pesos del modelo
cnn.load_weights(pesos_modelo)

# Función que se le pasa el path de la imagen y los dira que es.
def predict(file):
  # Se carga la imagen con la longitud y altura definida  
  x = tf.keras.preprocessing.image.load_img(file, target_size=(longitud, altura))
  # Convierte la imagen en una array de valores
  x = tf.keras.preprocessing.image.img_to_array(x)
  # Lo que hace es añadir una nueva dimensión en el eje 0
  x = np.expand_dims(x, axis=0)
  # Hacemos la predicción en base a la imagen pasada convertida en una array. 
  # Esto devuelve una array de 2 dimensiones tal que así: [[1,0,0]]
  array = cnn.predict(x)
  # Recogemos el valor de la dimensión 0 que es la que obtiene el resultado
  result = array[0]
  # Se mira en que indice estará el valor más alto. Como el modelo trabaja con 0 y 1 (por ello
  # se divide la imagen en 255). Donde este el 1, será la categoria que se corresponde a la imagen
  answer = np.argmax(result)
  if answer == 0:
    print("pred: Gato")
  elif answer == 1:
    print("pred: Gorila")
  elif answer == 2:
    print("pred: Perro")

  return answer


predict('K:/github/python_test/tensorflow/clasificador_imagenes/cat.4928.jpg')