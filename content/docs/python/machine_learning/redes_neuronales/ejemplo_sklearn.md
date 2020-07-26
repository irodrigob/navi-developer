---
title: Ejemplo de redes neuronales
description: Ejemplo de redes neuronales usando Sklearn
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 6. Redes neuronales](https://www.youtube.com/watch?v=7wC9YDImpyY).

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

En este ejemplo se usa la librería *Scikit learn(Sklearn)* que según el video no es lo mejor para ello. Hay que mejores como *TenserFlow* pero requiere de más conocimientos de algebra lineal.

# Código

[Código fuente descargable](/docs/python/machine_learning/redes_neuronales/redes_neuronales_sklearn.ipynb) 

Librerias del proceso
```tpl
import sklearn 
# Set de datos
from sklearn.datasets import load_iris
# Separación de datos de entrenamiento y test
from sklearn.model_selection import train_test_split
# Clasificador de la red neuronal
from sklearn.neural_network import MLPClassifier
```
Datos para pasarle al clasificador
```tpl
# Carga de datos
iris = load_iris()
```
Características de los datos
```tpl
# Datos con las características
caract = iris.data
```
Etiquetas de los datos
```tpl
# Datos con las etiquetas
etiq = iris.target
```
```tpl
# Separación de los datos de test y entrenamiento
X_ent, X_test, y_ent, y_test = train_test_split(caract, etiq)
```
Creación de la red neuronal
```tpl
# Se asigna el model de la red neuronal
# max_iter indica que las veces en que los nodos se van a intercambiar información 
# para aprender solo va ocurrir 10 veces.
# hidden_layer_sizes indica que va una capa oculta que tendrá 10 nodos.
red = MLPClassifier(max_iter=10, hidden_layer_sizes=(10))
```
Entrenamiento
```tpl
# Se entra al algoritmo
red.fit(X_ent, y_ent)
```
Resultado
```tpl
# Resultado del entrenamiento
red.score(X_test,y_test)
```
En este ejemplo devuelve un 28% predicciones correctas, es un valor muy malo. Para mejorarlo se puede subir el valor de *MAX_ITER* a 100, con que el % de aprendizaje sube al 81%. Si aumentamos a 500 el valor de *MAX_ITER* el % sube al 97%.

Hay que tener en cuenta que el set de datos de Iris es pequeño y al aumentar el número de iteracciones no afecta mucho al tiempo total de calculo. Contra más datos se tenga y más iteracciones se hagan más tiempo consumaría el algoritmo de aprendizaje.

En *HIDDEN_LAYER_SIZES* se le puede pasar un valor de *10,10*. Esto quiere decir que habrá 10 capas ocultas y en cada una de ellas 10 nodos.

Hay que tener en cuenta que si el resultado del entrenamiento es del 100% es que muy posiblemente tenga un sobreajuste.