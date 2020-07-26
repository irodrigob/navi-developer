---
title: Entrenar algoritmo de regresion lineal
description: Ejemplo de entrenamiento de algoritmo de regresion lineal
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 2. Algoritmos de regresión](https://www.youtube.com/watch?v=38rBECdCv3A&t)

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas

# Código

[Código fuente descargable](/docs/python/machine_learning/regresion_lineal/entrenar_algoritmos_regression_lineal.ipynb) 

Se cargan las librerias
```tpl
# Regresor de KNN o vecinos cercanos
from sklearn.neighbors import KNeighborsRegressor
# Set de datos de boston
from sklearn.datasets import load_boston
# Divide los datos entre entrenamiento y testing
from sklearn.model_selection import train_test_split
# Algoritmos de regresión linea y ridge
from sklearn.linear_model import LinearRegression, Ridge
```

Carga del modelo de datos que se van a usar
```tpl
# A la variable boston se le asigna el set de datos. Que son los precios de las 
# casas en boston que depende de varías características
boston = load_boston()

# Visualiza las claves que tiene los datos
boston.keys()
```

Son sentencias que permite los datos del modo cargado en el paso anterior. 
# Si se descomenta las sentencia y se ejecuta se van viendo los datos
```tpl
# La data son las características
#boston.data

# Respuestas a nuestras características
#boston.target

# Ver los ejemplos que hay en data. Que devuelve 506, 13. Que son 506 casa 
# con 13 características
#boston.data.shape

# Si se hace lo mismo pero con el target devuelve 506 respuestas
#boston.target.shape
```

Variables que se usarán para el entrenamiento y test
```tpl
# Las que terminan en "_ent" son para entrenar y las que terminan 
# en "_test" son para testing. 
# Estas variables se inicializan con la data o características y las etiquetas
X_ent, X_test, y_ent, y_test = train_test_split(boston.data, boston.target)
```

Permite ver que se datos se usan para entrenamiento y para test
```tpl
# Si hacemos esto se que son los datos que ha usado para entrenar. Que serán 379, 13
print("Datos para entrenar:",X_ent.shape)
# Y para testing devuelve el resto 127, 13
print("Datos para testing:",X_test.shape)
# resto de variables que son los vector pero sin las características. 
# Solo devuelve 379 para _ent y 127 para _test
print("Datos y para entrenar:", y_ent.shape)
print("Datos y para testing:", y_test.shape)
```

Primer algoritmo de regresion líneal.
```tpl
# Variable que se le asocia con el algoritmo de vecinos cercanos. Pasándole 
# que considere 3 vecinos.
knn = KNeighborsRegressor(n_neighbors=3)
```

Datos de entrenamiento
```tpl
# Se le pasa al algoritmo los datos de entrenamiento y los valores 
# objetivos o guia. Al ejecutar se visualizará los parámetros
# del algoritmo que han sido modificados, como es el número de vecinos.
knn.fit(X_ent,y_ent)
```

Resultado del entrenamiento
```tpl
# Para saber como aprendio nuestro algoritmo se le pasan los 
# datos de test. 
# Si se le pasa 3 vecinos el % de aprendizaje es un 48%.  Pero si se le 
# sube el valor del parámetro n_neighbors a 5 el % de aprendizaje 
# disminuye al 47%. 
# Y si ponemos 4 es un poco inferior al 3. Por ello, el más optimo 
# es dejarlo en 3 para este set de datos.
knn.score(X_test,y_test)
```
Borrao de datos para probar otros algoritmos.
```tpl
# Se borra el contenido de la variable para poder usar la libreria
# "LinearRegressionLinearRegression"
# El objetivo de borrar es evitar que la maquia se sature al procesos varios modelos
del knn
```

Se repiten los mismos pasos para usar otro algoritmo.
```tpl
rl = LinearRegression()
```
```tpl
# Se alimenta con los mismos datos que para el algoritmo de vecinos cercanos
rl.fit(X_ent,y_ent) 
```
```tpl
# Vamos a ver cual ha sido su nivel de aprendizaje. Pasandole 
# los mismos datos que para el KNN. En este caso ha aprendido un 72% 
# mucho mejor que el 48% del KNN
rl.score(X_test,y_test)
```
```tpl
# Ahora se va borra el algoritmo usado para utilizar el algoritmo de ridger
del rl
```
Lo mismo pero para el algoritmo ridge
```tpl
ridge = Ridge(alpha=1)
```
```tpl
# Se vuelve alimentar con los mismo datos de los algoritmos anteriores
ridge.fit(X_ent, y_ent)
```
```tpl
# Y ahora a ver cuanto ha aprendido
# Devuelve un 72%(redonde hacia arriba) que es un poco mejor al 
# algoritmo de regression lineal. Cambiando el parámetro alpha a 0.5 
# devuelve un valor sensible inferior al valor por defecto que es alpha = 1
ridge.score(X_test,y_test)
```
```tpl
del ridge
```