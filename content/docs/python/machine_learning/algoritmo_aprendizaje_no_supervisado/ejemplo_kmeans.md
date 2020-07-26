---
title: Ejemplo KMeans
description: Ejemplo Kmeans
---

# Introducción

Ejemplo extraído de [Machine Learning episodio 8. KMeans](https://www.youtube.com/watch?v=8aUqS3Ge4Q4).

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

Como ejemplo para ver el funcionamiento de Kmeans se va utilizar el set de datos de *iris*. Pero en este caso solo se le va a pasar las características pero no las etiquetas para que clasifique en tres grupos los datos. Luego se le pasarán las etiquetas para ver como ha aprendido el algoritmo, pero no se pasarán las etiquetas para su aprendizaje.

En este ejemplo es un ejemplo de como este tipo de algoritmo, como se explica en el índice, es de prueba y error. Ya que variando el número de cluster o iteracciones la capacidad de aprendizaje aumenta o disminuye. En este tipo de cada lo mejor es poner el algorimo dentro de un FOR para que se pueda ir viendo cual es la combinación más idónea.

Una vez de las ventajas del set de datos de *iris* es que tiene a que etiqueta pertnece cada flor y es fácil hacer comprobaciones. En caso de no disponer dicho valor comparativo entonces casí hay que meterse en cada valor para ir viendo como lo clasifico. Por ejemplo, si en el set datos hubiese una fecha, de cuando se inicio la medición, posiblemente estaría agrupando por dicho campo y afectaría a la clasificación. En este caso habría que eliminar dicha información para mejor el proceso. 

Este tipo de algoritmo es mucho de prueba y error e ir interpretando los datos para ir puliendolos para que el algoritmo clasifique mejor.


# Código

[Código fuente descargable](/docs/python/machine_learning/alg_aprend_no_supe/ejemplo_kmeans.ipynb) 

```tpl
# Algoritmo aprendizaje no supervisado
from sklearn.cluster import KMeans
# Datos para el entrenamiento
from sklearn import datasets
# Libreria para ver que tal aprendido el algoritmo
from sklearn import metrics
```
```tpl
# Datos de iris
iris = datasets.load_iris()
```
```tpl
# Características de los datos. 
# Cada fila es una flor y cada columna es el grosor o longitud del petalo y cepalo
X = iris.data
# Etiquetas de los datos. Que indica a que especie pertenece cada flor de los datos anteriores.
Y = iris.target
```
```tpl
# Datos de las carácterísticas
print("Características: ", X)
```
```tpl
# Datos de las etiquetas
print("Etiquetas: ", Y)
```
```tpl
# Algoritmo KMeans. Donde se le indica:
# que dividida los datos en dos grupos o cluster. Nota el set de datos tiene 3 grupos
# pero es para se vea la diferencia.
# Y haga 3000 iteracciones para ajustar los centroides lo máximo posible
km = KMeans(n_clusters=4,max_iter=3000)
```
```tpl
# Entrenamiento del algoritmo
km.fit(X)
```
```tpl
# Es la predicción que indicará a que grupo pertenece los datos pasados.
predicciones = km.predict(X)
```
```tpl
# Predicciones que realiza. El valor 0 indica el primer grupo, el 1 al segundo y así sucesivamente
print("Predicción realizada por el algoritmo: ", predicciones)
```
```tpl
# Variable para que compare la predicción realizada versus las etiquetas de los datos
score = metrics.adjusted_rand_score(y, predicciones)
```
```tpl
# Resultado de hace 2 cluster y 3000 iteracciones arroja un resultado del 53% de acierto.
# Si se cambia el algoritmo a 3 cluster el % sube al 73%
# Si el número de cluster se sube a 4, el % disminuye al 65%. 
# Con lo cual el número de grupos/cluster idónea es 3.
print("Comparativa etiqueta vs predicción: ", score)
```