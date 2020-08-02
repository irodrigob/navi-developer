---
title: Redes convolucionales
description: Redes convolucionales
---

# Introducción

Ejemplo extraído de [Redes neuronales convolucionales CNN (Clasificación de imagenes)](https://www.youtube.com/watch?v=ns2L2T6wvAY). Se pondrán imagens obtenidas del video para explicar mejor el funcionamiento tal como lo hace en el video.

Como se explico en la página de [tipos de redes neuronales](/docs/python/machine_learning/redes_neuronales/tipos_redes_neuronales.md) este tipo de red nace para procesar de una manera eficiente imagenes, aunque también se usa para procesamiento de texto. Pero realmente su fuerte es el procesamiento de imagenes.

# Explicación

La estructura de este tipo de redes es la siguiente:

![Estructa](/images/python/machine_learning/redes_neuronales/red_cnn_estructura.png)

Dentro de las capas ocultas se hacen dos tipos de operaciones:

1. *Pooling* o agrupación. Que lo hace es reducir el tamaño de la imagen para que cuando se pase a la siguiente capa sea menos pesado procesarla:

![Pooling](/images/python/machine_learning/redes_neuronales/red_cnn_pooling.png)

2. *Convoluciones*. Diriamos que ir pasandole filtros a nuestra imagen para ir detectando ciertos patrones en la imagen:

![Convolución](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion.png)

Para entender mejor como funcionan vamos con el problema de clasificar imagenes. Se le pasa un animal y se tiene que identificar que es: perro, ave, etc.

Por ejemplo tenemos la siguiente imagen de entrada:
![Imagen de entrada](/images/python/machine_learning/redes_neuronales/red_cnn_imagen_ejemplo.png)

En una red neuronal DNN se mirará la longitud, altura y la profundidad (son 3 capas ya que hay 3 canales colores: RGB). Primero lo que haría es calcular el número de conexiones, si asumimos que la imagen es de 100x100 pixeles, que hay que multiplicar * 3 por el número de capas de colores, y la capa oculta tiene 100 neuronas. Esto haría 3 millones de conexiones. Además, este tipo de red no aprendería bien los animales, ya que lo que queremos es que si el animal esta arriba, abajo o donde sea de la imagen sea capaz de identificarlo. Y eso la red DNN no son capaces de hacerlo. Es cuando las redes CNN son más eficientes.

Las red CNN no tomará de inicio los pixeles de la imagen y los comenzaría a procesar. Sino que va a empezar a convolucionar la imagen, es decir, para ir procesando la imagen por zonas. De esta manera al no procesar la imagen en su totalidad el número de conexiones es mucho menor. Lo que va haciendo en cada convolución:

![Convolución paso 1](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion_paso1.png)
![Convolución paso 1](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion_paso2.png)
![Convolución paso 1](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion_paso3.png)

Es ir procesando partes de la imagen para generando nuevas imagenes de salida:

![Resultado de la imagen](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion_resultado_imagen.png)

Donde serán más pequeñas tanto en el ancho como en el alto pero va aumentar en profundidad.

El siguiente paso es el *pooling*. Este paso aún va a reducir más la altura y la anchura de la imagen pero va dejar intacta la profundidad.

Los parámetro importantes de la capa convolucional son:

1. Tamaño del filtro. Es el tamaño de la imagen que se va ir procesando en cada capa. Se indicará tanto la anchura como la altura.
2. Profundidad de la capa. Es el número de filtros que se le va aplicar a nuestra imagen. 

![Profundida de la capa](/images/python/machine_learning/redes_neuronales/red_cnn_convolucion_profundidad_capa.png)

En la imagen el filtro 1 sería la capa roja, el filtro 2 la capa azul, el filtro 3 la capa verde y el filtro 4 la capa amarilla. Cada filtro que se aplica se genera una nueva imagen. Cada filtro se aplica sobre la imagen original pero aplicando otros patrones de búsqueda para detectar cosas que no han sido detectadas por los filtros anteriores. Un primer filtro puede detectar bordes, un segunfo filtro, sombras, etc. 
Estos filtros son los que van a ir incrementando la profundida de la imagen, ya que son el número de filtros que se van aplicando.

3. Stride o Paso. Es como se va ir recorriendo la imagen en cada filtro.

![Stride](/images/python/machine_learning/redes_neuronales/red_cnn_stride.png)

Es decir, cuantos pixeles, longitud y altitud, se van a ir procesando en cada filtro. Hay que tener en cuenta que contra más sea el *paso* más se va a reducir la imagen.


El paso final de la convolución es el *pooling*. Este paso ya se ha indicado que reduce todavía más el tamaño de la imagen. El motivo son dos:

1. Para quitar el número de conexiones y no sea tan pesado procesar la imagen.
2. Y ayuda a no sobreajustar el modelo.

Dentro del *pooling* hay dos tipos:

1. *Maxpooling*. 
2. *average pooling*

Ambos lo que hacen toman los valores indicados en la longitud y latitud del filtro para:

![Average y Max pooling](/images/python/machine_learning/redes_neuronales/red_cnn_max_average_pooling.png)

En maxpooling toma el valor más alto de cada zona leída por el filtro. Mientras que el avergage toma el promedio de los valores de los pixeles. El resultado sirve para componer la siguiente imagen.

El orden de ejecución es siempre primero *convolución* y segundo el *pooling*, así por cada capa oculta de la red. 

A medida que la red va llegando al final la *convolución* va aplicando filtros cada vez más elaborados. En los primeros filtros de la red se detectarán rayas, sombras colores pero a medida que avanza va identificando elementos más elaborados:

![Identificación pasos](/images/python/machine_learning/redes_neuronales/red_cnn_identificacion_pasos.png)
