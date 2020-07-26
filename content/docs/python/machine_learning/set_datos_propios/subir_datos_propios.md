---
title: Subir datos propios
description: Subir datos propios
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 11. ¿Cómo subir tu set de datos propio?](https://www.youtube.com/watch?v=wTA8nE-BJGc). 

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

# Explicación

En el video se muestra un excel con los datos *iris* con multidud de valores. Para este página se ha creado un [excel](/docs/python/machine_learning/set_datos_propios/set_datos_propio.xlsx) sencillo con la misma estructura pero con datos limitados e inventados. Y se ha conviertido a [CSV](/docs/python/machine_learning/set_datos_propios/set_datos_propio.csv) para que pueda importado por la librerías de machine learning.
El fichero tiene ocho columnas de características y una columna para la etiqueta.


# Código

[Código fuente descargable](/docs/python/machine_learning/datos_propios/set_datos_propio.ipynb) 

```tpl
# Paquete para leer el fichero CSV
import pandas as pd
# Algoritmo de regresión línea
from sklearn import linear_model
# Separador de datos de test y entrenamiento
from sklearn.model_selection import train_test_split
```
```tpl
# Algoritmo de regresión
reg = linear_model.LogisticRegression()
```
```tpl
# Datos con el modelo
archivo = "set_datos_propio.csv"
```
```tpl
# Lectura del modelo. 
# Hay que indicarle el separador para que funcione.
df = pd.read_csv(archivo,';')
```
```tpl
# Visualización de los datos cargados
df
```
Este bloque no esta igual que el ejemplo orignal porque el método *as_matrix* ya no existe y se usa su equivalente.
```tpl
# Arreglox contendrá todas las características. 
# El -1 es para que no se lea la última columna 
# que es la etiqueta
#print("Valores: ", df.columns[0:-1])
arreglox = df[df.columns[:-1]].to_numpy()
# Arregloy contendrá la ultima columna, que es la etiqueta
arregloy = df[df.columns[-1]].to_numpy()
```
```tpl
print("Características: ",arreglox)
```
```tpl
print("Etiquetas: ", arregloy)
```
```tpl
# Separacion de datos de entrenamiento y test
X_ent, X_test, y_ent, y_test = train_test_split(arreglox, arregloy)
```
```tpl
# Entrenamiento del algoritmo
reg.fit(X_ent, y_ent)
```
```tpl

```
```tpl
# Resultado del entrenamiento
reg.score(X_test, y_test)
```