---
title: Usar dataset
description: Usar datasets
---

# Introducción

Este articulo lo he tenido que hacer para aprender la base de como funciona los dataset en Tensor Flow, porque si no hay manera de entender los ejemplos más complejos. O ejemplos creados en versión 1.x de Tensor Flow que al usarlos en la versión 2.0 hay que migrarlos.


# Libreías de cada ejemplo

Las librérías que uso en cada ejemplo son las sigueintes:
```tpl
# Libreria tensor flowe
import tensorflow as tf
# Daset de datos con números escritos a manorom 
from tensorflow.examples.tutorials.mnist import input_data
import tensorflow_datasets as tfds
# API de tensorfow para construir y entrenar modelos de alto nivel
from tensorflow import keras
```

# Mecánica básica

[Código fuente descargable](/docs/python/machine_learning/tensor_flow/tf_basico_datasets.ipynb) 

```tpl
# Creamos una set de datos que es un array de seis elementos
dataset = tf.data.Dataset.from_tensor_slices([8, 3, 0, 8, 2, 1])
```
```tpl
# Para listar los ejemplos del dataset hay que 
# recorrelos con un FOR.
# Cada elemento del dataset es de tipo numpy. Por eso hay que usar
# "numpy" para obtener el valor.
for elem in dataset: 
  print(elem.numpy())
```
Resultado:
```
8
3
0
8
2
1
```

```tpl
# Ahora vamos crear un dataset de tipo matriz. 
# Que tendrá 4 filas y 10 columnas
dataset1 = tf.data.Dataset.from_tensor_slices(tf.random.uniform([4, 10]))
```
```tpl
# Para ver los elementos:
for elem in dataset1: 
  print("Fila: ",elem.numpy())
```
Resultado:
```
Fila:  [0.58600104 0.80292606 0.13899899 0.6027132  0.2812158  0.06768107
 0.01700175 0.4155041  0.42036211 0.94815934]
Fila:  [0.09541941 0.7888255  0.5253123  0.960852   0.962497   0.5518899
 0.16105258 0.98514235 0.82917464 0.5447879 ]
Fila:  [0.30786347 0.2631991  0.83314383 0.4485308  0.992118   0.17097425
 0.22812164 0.20294225 0.95297885 0.30839372]
Fila:  [0.7794962  0.05844533 0.678041   0.50282586 0.60348916 0.94892263
 0.3606732  0.57894313 0.08367121 0.97359645]
```
```tpl
# El siguiente ejemplo es lo más parecido al dataset de minst. 
# Ya que tiene dos niveles uno como un array de 4 posiciones y otro una matriz de 4 filas y 100 posiciones
dataset2 = tf.data.Dataset.from_tensor_slices(
   (tf.random.uniform([4]),
    tf.random.uniform([4, 100], maxval=100, dtype=tf.int32)))
```
```tpl
# Se recorre el dataset creado, como tiene dos elementos en el FOR
# se recupera ambos valores.
for elem1, elem2 in dataset2: 
  print(elem2.numpy())
```
Resultado
```
[84 89 29 72 34 83 85 67 81 48 41 10 61 91 17 83 41 69 68 82 74 96  1 61
 96 55 41 20 57 89 96 78 75 62 66 18  2 80 32 11 95 88 69 97 36 84 78 83
 68 59 11 72 29 89 23 53 78 91 51 24 37 59 32 18 57 74 50 42 76  0 55 24
 50 45 70 85  1 56 32 70 15 50 36 23 68 22 93 52 54 86 91 62 93 21 83 77
 16 61 83 29]
[48 76 66 53 96 95 78 40 25 11 64 83 89 15 72 15 63 46 37 44 50  2 26 92
  7 79 73 49 34 53 35 81 85 66 55 23 52  2 78 52 55 23  2 87 86  0  4 29
 50  4  6 80 75 86 98 96  1 61 65  4 20 84 61 94  6 35 94 84 46 13 45  0
 51 34 50 18 56 98 81 16 51 20 69 97 65 25 40 31 59 90 52 94 58 65 79 45
 95  2 25 28]
[36 71 19 46 22 71 36 75 53 60 47 90 82 23 70 14 41 57 53 69 69 59 76 39
 75 78 21 83 80 55 16 25 96  1 74 13 74 27 42  8  8 59 52 94 76 98 74 53
 80 61 19 76  7 42 77 78 61 52 51 39 67 49 84 34 30  9 73 56 57 11 70 32
 33 81 60 99 54 52 18 28 71 60 65 46 87 38 62  0 14 98 37 26 32  0 15 50
 16 19 82 58]
[63 54 20 24 36 54 90 14 89 89 13 38 44 73 49 22 78 47 72 89  1 92 38 78
 61 48 55 34 18 72 54 92 95 12 61 84 65 49  7  3 65 97 46  0 21 69 77 98
 94 12 91 16 21 67 60 46 83 43 12 55 45 78 73 53 42 91 37 88 86 46 92 67
 65 14 71 37 85 35 63 45 55 51 73 55 26 15 45 42 61 29 99 39 91 19  7 17
 74 95  6 88]
```
Resultado al mostrar *elem1*
```
0.2338922
0.64260435
0.48011637
0.24890351
```

# Mecánica batch

[Código fuente descargable](/docs/python/machine_learning/tensor_flow/tf_basico_datasets_batch.ipynb) 

El proceso batch es muy útil cuando hay un gran volúmen de datos en el dataset. Esto permite ir leyendo los datos en bloques y procesandolos:

Tomamos como base el ejemplo anterior:
```tpl
# Creamos una set de datos que es un array de seis elementos
dataset = tf.data.Dataset.from_tensor_slices([8, 3, 0, 8, 2, 1, 20])
```
```tpl
# Los dataset se pueden procesar en lotes, batch.
# En la siguiente convertimos nuestro dataset en batch
# indicando que lotes sean de 3 en 3
dataset_batch = dataset.batch(3)
```
```tpl
# Se recorre los set de datos. El take indica cuantos
# veces se van a leer datos
for batch in dataset_batch.take(100):
  print([arr.numpy() for arr in batch])
```
Con el valor 100 del ejemplo el resultado es el siguiente:
```
[8, 3, 0]
[8, 2, 1]
[20]
```
Si pone el valor 1 al *take* el resultado es el siguiente
```
[8, 3, 0]
```
En el siguiente se indica como se calculará el valor a pasar
a la función *taken* para saber que valor hay que pasarle
para recuperar todos los datos
```tpl
# Vamos a ver cuantos pasadas hay que hacer para leer los datos
# Genera un array con los valores que se leen cada pasada.
batch_sizes = [batch.shape[0] for batch in dataset_batch]
batch_taken = len([batch.shape[0] for batch in dataset_batch])
print("Número de datos que se leen en cada pasada:", batch_sizes)
print("Número valores a pasar a taken: ", batch_taken)
```
Resultado:
```
Número de datos que se leen en cada pasada: [3, 3, 1]
Número valores a pasar a taken:  3
```

Cuando se entrena algoritmo de redes neuronales se hacen varias pasadas para al algoritmo para mejorar su entrenamiento. Ese concepto se llama *epochs*, el esqueleto básico sería el siguiente ejemplo:
```tpl
for epoch in range(3):
    print("Inicio época: ", epoch)
    for batch in dataset_batch.take(100):
        print([arr.numpy() for arr in batch])
    print("Fin epoca: ", epoch)
```