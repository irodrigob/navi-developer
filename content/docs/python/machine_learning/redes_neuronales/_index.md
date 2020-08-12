---
title: Redes neuronales
description: Redes neuronales
bookCollapseSection: true
---

# Introducción

Todo sobre redes neuronales

# Que son

Las redes nacen para poder emular el cerebro humano. Lo que se intentar es construir nodos en una máquina, y que estos nodos esten comunicados y se transfieren información entre ellos.

# Como funciona el algoritmo

Ejemplo extraído de [Machine Learning episodio 6. Redes neuronales](https://www.youtube.com/watch?v=7wC9YDImpyY). Se pondrán imagens obtenidas del video para explicar mejor el funcionamiento tal como lo hace en el video.

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

# Secciones

Las secciones son las siguientes:

{{<section>}}