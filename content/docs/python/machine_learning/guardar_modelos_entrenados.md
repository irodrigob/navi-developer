---
title: Guardar modelos entrenados
description: Guardar modelos entrenados
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 9. Cómo guardar un modelo entrenado](https://www.youtube.com/watch?v=5X3xWlJ2Ozw).

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

En esta página veremos como se guardan los modelos entrenados para no tener que hacer entrenamientos cada vez que queremos hacer algo. En los ejemplos entrenar algoritmos se hace en segundos, pero en casos reales este entrenamiento puede durar dias o semnanas. Por eso es vital guardar el entrenamiento. En el video lo llamán *Persistencia del modelo*. 

La página la pongo a nivel general dentro de machine learning porque es aplicable a todos los ejemplos.

** NOTA: La librería tal como se ve en el video es obsoleta. He puesto la manera en que se grabaría actualmente**

# Código guardar el modelo entrenado

```tpl
# Carga de datos de ejemplo y algoritmo de regesion logísitca
from sklearn import datasets, linear_model
# Separación de datos de test y reales
from sklearn.model_selection import train_test_split
# Guardar entrenamiento a fichero
from joblib import dump, load
```
```tpl
# Datos de las flores
iris = datasets.load_iris()
# Algoritmo de regresion líneal
clf = linear_model.LogisticRegression()
```
```tpl
# Se muestra las claves de los datos. Esas claves es la que 
# se usa para poder entrenar el algoritmo
print("Claves de los datos: ", iris.keys())
```
```tpl
# Separación de datos de entrenamiento y test
X_ent, X_test, y_ent, y_test = train_test_split(iris.data, iris.target)
```
```tpl
# Entrenamiento del modelo
clf.fit(X_ent, y_ent)
```
```tpl
# Resultado del entrenamiento
clf.score(X_test, y_test)
```
```tpl
# Se guarda el fichero con el entrenamiento
dump(clf, "modelo_entrenado.pkl")
```

# Código para recuperar modelo entrenado
```tpl

```


```tpl

```