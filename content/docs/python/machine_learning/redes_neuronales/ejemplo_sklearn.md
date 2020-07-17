---
title: Ejemplo de redes neuronales
description: Ejemplo de redes neuronales usando Sklearn
---

# Introducción

Ejemplo extraído del video [Machine Learning episodio 6. Redes neuronales](https://www.youtube.com/watch?v=7wC9YDImpyY)

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

En este ejemplo se usa la librería *Scikit learn(Sklearn)* que según el video no es lo mejor para ello. Hay que mejores como *TenserFlow* pero requiere de más conocimientos de algebra lineal.

En la siguiente imagen tenemos un ejemplo de diagrama de una red neuronal:

![Diagrama red neuronal](/images/python/machine_learning/redes_neuronales/diagrama_red_neuronal.png)

En este diagrama tenemos cuatro capas de nodos interconectados entre ellos. 

Los nodos de la izquierda del todo son los datos son los que se van a usar para alimentar el algortimo.

Las capas ocultas son aquellas capas que no son ni de entrada ni de salida. Se llaman así porque se no se saben muy bien lo que hacen, solo que se pasan información entre ellas para ir alterando los valores para obtener predicciones más acertadas en el futuro.

Las capas de salida son las que devolverán los datos del algoritmo. En el caso de los datos de prueba de *Iris*(clasificación de las flores) serían el tipo de flor a la que pertenece.

Cada nodo del diagrama tiene un peso y un valor que se lo irá pasando al resto de nodos. Por ejemplo si estamos en el primer nodo de la segunda capa (la de la capa oculta), este nodo va a recibir información de los cuatro nodo de entrada. La información que recibe se divide en dos: Por un lado el valor que es la *X*, de la formula de la imagen, y su peso que es la *W* al que se le multiplará a *X*. A ese valor, que indicará que tan importante es dicho valor, se le sumará *B*. *B* es el *bias*, o sesgo, en castellano. Esto indicará que tan propensa es el nodo a activarse, como una especie de interruptor.

En este ejemplo:

![Formula de activación](/images/python/machine_learning/redes_neuronales/formula_activacion_nodo.png)

indica que *Y* se activará si el valor obtenido supera el *0,5*. Con los valores de la imagen el nodo no se activará y no enviará información a las siguientes capas.

Ahora si cambiamos los valores que recibe por los siguientes:
![Formula de activación 2](/images/python/machine_learning/redes_neuronales/formula_activacion_nodo2.png)

El nodo si que se va activar, y enviará información a la siguiente capa.

Volviendo al diagrama de la red. Lo que hace el algoritmo de entrenamiento es como sabe las respuestas que debe esta recibiendo es ir modificando los valores de *W* y *B* para que las últimas capas de salida se activen cuando sea necesario. De esta manera es como el algoritmo aprende.

# Código

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