---
title: Ejemplo de conversión de palabras a números
description: Ejemplo de conversión de palabras a números
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 7. Vectorización](https://www.youtube.com/watch?v=9IqWxJ1T57U).

Partimos de las siguiente dos frases:

1. Vectorizar el texto es necesario en machine learning
2. Machine learning es una gran herramienta

El primer pasar es tokenizar. Tokenizar es dividir el texto en tokens o palabras.

Los token los obtendremos de las palabras no repetidas que son:

1. Vectorizar
2. el
3. texto
4. es
5. necesario
6. en
7. machine
8. learning
9. una
10. gran
11. herramienta

*Nota: Se cuenta como misma palabra "Machine" que "machine"*

Lo siguiente es contar el número de tokens obtenidos que son 11.

A continuación se va a crear dos vectores, uno por cada frase, que tendrá como tamaño el número de palabras o tokens obtenidos. Y a cada posición se le va asignar el número de veces que se repite una palabra en la frase según su posición en el token obtenido no en la frase.

El vector de la primera fase sería:

* [1,1,1,1,1,1,1,1,0,0,0] --> Las primeras 8 palabras de la frase solo aparece una vez en el token obtenido, mientras que las tres últimas no aparecen.

El vector de la segunda frase sería:
* [0,0,0,1,0,0,1,1,1,1,1] --> En la 4 posición aparece un 1 es porque el token número 4 contiene el valor *es* que se repite una vez en la segunda frase. Lo mismo para el resto de 1. En la posición 7 del vector se correspondería a la palabra *machine* que se repite una vez en la segunda frase.

Según como se ve en la vectorización se van a tener vectores muy similares cuando las frases son muy parecidas. Por ejemplo si hablamos de *Machine learning* vamos a ver que las posición 7 y 8 van a estar activados, de esta manera se van a encontrar similitudes en ambos.

Es muy parecido a lo que hace con el juego de datos de *Iris* pero en este caso las características van a ser las palabras.

En el ejemplo siguiente debido al volúmen de datos del set importado hay palabras extrañas. Lo ideal será limpiar los datos de estas palabras extrañas para evitar que procesen, para ello lo mejor
es usar la  [libreria NLTK](/docs/python/machine_learning/librerias.md) para ello. Con esta librería se preprocesarían dichas palabras para dej


# Código

[Código fuente descargable](/docs/python/machine_learning/vectorizacion/vectorizacion_palabras.ipynb) 

Librerias de ejemplo
```tpl
import sklearn 
# Set de datos que viene con las características: Un cuerpo de texto y con etiquetas 
# la categoria a la cual pertenece. La categoria es de que tema se esta hablando.
from sklearn.datasets import fetch_20newsgroups
# Separación de datos de entrenamiento y test
from sklearn.model_selection import train_test_split
# Algoritmo que va permitir vectorizar todo el texto que tenemos.    
from sklearn.feature_extraction.text import CountVectorizer
# Algoritmo de regresión logísticas para la clasificaci´
from sklearn.linear_model import LogisticRegression
```
Set de datos
```tpl
# Set de datos
noticias = fetch_20newsgroups(subset="train")
```
Ejemplo de datos
```tpl
# Podemos ver un ejemplo de datos que se va a procesar. Que se verá que es un corre electrónico
print("Cuerpo del mensaje: ", noticias.data[0])
# Si hacemo los mismo para ver a que clasificación pertenec. Aparecerá el valor 7, que es la clasificación a la que pertenece.
print("Clasificación: ", noticias.target[0])
```
Número de bloques de texto
```tpl
# Cuantos bloques de texto hay en el modelo de datos
print("Número de bloques de texto que hay: ", len(noticias.data))
```
Clasificaciones del set de datos
```tpl
# Nombre de las clasificaciones
print("Nombres de las clasificaciones:", noticias.target_names)
```
Variable para la vectorización
```tpl
# Variable para la vectorización
vector = CountVectorizer()
```
Datos para la vectorización
```tpl
# Se le pasan los datos que tenemos en la variable vector.   
vector.fit(noticias.data)
```
Datos generados en el proceso de vectorización
```tpl
# Se visualizan los tokens generados de los datos pasados.print
# Un token = una palabra única en todo el set de datos.
print("Tokens generados: ", vector.vocabulary_)
```
Variable con el vector que contiene las repeticiones de cada fila de datos
```tpl
# Se crea una variable con la bolsa de palabras. Esta bolsa es el vector donde se cuenta el número de repeticiones que tienen cada una de las palabras
bolsa = vector.transform(noticias.data)
```
Resultado de la vectorización
```tpl
# Resultado de la matriz.
# Devuelve un valor de 11314, 130105, 
# 11314 es el número de datos, o filas, que hay en el set de datos. 
# Y por cada fila tiene 130107 elementos, que es el número de palabras
# distintas que hay en el modelo.
print("Filas de datos x columnas: ", bolsa.shape)
```
Clasificaciones para el algoritmo
```tpl
# Ahora se crea la variable que contiene las clasificaciones
bolsay = noticias.target
```
Datos para entrenar y test
```tpl
# Se separán los datos para entrenamiento y test.
X_ent, X_test, y_ent, y_test = train_test_split(bolsa, bolsay)
```
Algoritmo de regresión
```tpl
# Algoritmo de regresion
lr = LogisticRegression()
```
Resultado del entrenamiento
```tpl
# Se le pasan los datos para el entrenamiento
lr.fit(X_ent, y_ent)
```