---
title: Librerias y paquetes
description: Librerias y paquetes
weight: 20
---

# Introducción

Librerias y pawuetes para poder usar el Machine Learning.

Los pasos para instalar las librerías esta explicado en la página de [Anaconda](/docs/python/anaconda.md).

# Numpy

## ¿Qué és?

Según la Wikipedia: *NumPy es una extensión de Python, que le agrega mayor soporte para vectores y matrices, constituyendo una biblioteca de funciones matemáticas de alto nivel para operar con esos vectores o matrices.*

## Instalación

No se instala porque viene incluido con Anaconda:

![Numpy instalado por defecto](/images/python/machine_learning/anaconda/numpy_viene_instalado.png)

# Scikit learn(Sklearn)

## ¿Qué és?

Según la Wikipedia: *Scikit-learn es una biblioteca para aprendizaje automático de software libre para el lenguaje de programación Python.​Incluye varios algoritmos de clasificación, regresión y análisis de grupos entre los cuales están máquinas de vectores de soporte, bosques aleatorios, Gradient boosting, K-means y DBSCAN.*

## Instalación

Por defecto no se encuentra instalada. En la página que habla [Anaconda](/docs/python/anaconda.md) en el apartado de instalación de paquetes mediante entorno gráfico explica como se hace la instalación.

# Graphviz

## ¿Qué és?

Según la Wikipedia: *Graphviz es un conjunto de herramientas de software para el diseño de diagramas definido en el lenguaje descriptivo DOT.​ Fue desarrollado por AT&T Labs​ y liberado como software libre con licencie tipo Eclipse.​*

## Instalación

La instalación más sencilla es hacerlo a través de la interface gráfica de Anaconda:

![Libreria Graphviz](/images/python/machine_learning/anaconda/libreria_grahviz.png)

Y el paquete:

![Libreria Graphviz 2](/images/python/machine_learning/anaconda/libreria_grahviz2.png)

Para que funcione correctamente la libreria.

# Matplotlib

## ¿Qué és?

Según la Wikipedia: *Matplotlib es una biblioteca para la generación de gráficos a partir de datos contenidos en listas o arrays en el lenguaje de programación Python y su extensión matemática NumPy. Proporciona una API, pylab, diseñada para recordar a la de MATLAB.*

## Instalación

Esta libreria ya viene instalada por defecto con Anaconda y no es necesario realizar ningún instalación

# SVM (Support Vector Machine / Maquina de vectores de soporte)

## ¿Qué és?

Según la Wikipedia: *Las máquinas de vectores de soporte o máquinas de vector soporte (Support Vector Machines, SVMs) son un conjunto de algoritmos de aprendizaje supervisado desarrollados por Vladimir Vapnik y su equipo en los laboratorios AT&T.*
*Estos métodos están propiamente relacionados con problemas de clasificación y regresión. Dado un conjunto de ejemplos de entrenamiento (de muestras) podemos etiquetar las clases y entrenar una SVM para construir un modelo que prediga la clase de una nueva muestra.*

## Instalación

Viene incluida en la librería *Scikit learn(Sklearn)*

# NLTK

## ¿Qué és?

Según la Wikipedia: *El kit de herramientas de lenguaje natural, o más comúnmente NLTK, es un conjunto de bibliotecas y programas para el procesamiento del lenguaje natural (PLN) simbólico y estadísticos para el lenguaje de programación Python.*

Lo que hace el NLTK es eliminar lo que se llaman *stop words*. *stop words* son palabras mudas, o que no aportan valor para los algoritmos. Palabras como: en, la, el, una, etc. Son palabras que normalmente los algoritmos de búsqueda suelen ignorar.

## Instalación

La librería no esta instalada por defecto, lo más fácil es instalarla a través del anaconda navigator:

![Libreria NLTK](/images/python/machine_learning/anaconda/libreria_nltk.png)

# KMeans

## ¿Qué és?

Según la Wikipedia: *Es un método de agrupamiento, que tiene como objetivo la partición de un conjunto de n observaciones en k grupos en el que cada observación pertenece al grupo cuyo valor medio es más cercano.*

KMens se utiliza en algoritmo de aprendizaje no supervisado.

## Instalación

Viene incluida en la librería *Scikit learn(Sklearn)*

# PCA(Principal component Analysis)

## ¿Qué és?

Según la Wikipedia: *En estadística, el análisis de componentes principales (en español ACP, en inglés, PCA) es una técnica utilizada para describir un conjunto de datos en términos de nuevas variables («componentes») no correlacionadas. Los componentes se ordenan por la cantidad de varianza original que describen, por lo que la técnica es útil para reducir la dimensionalidad de un conjunto de datos.*

Esta librería se usa para reducir ruido de los datos que se van a procesar y ver los datos de multiples dimension en dos dimensiones para poder analizarlos.

## Instalación

Viene incluida en la librería *Scikit learn(Sklearn)*

# Mglearn

## ¿Qué és?

Realmente no es una librería, es una paquete que tiene funciones de ayuda para el libro *Introduction to Machine Learning with Python*. En la siguiente [dirección esta el detalle de dicho paquete](https://github.com/amueller/mglearn)

## Instalación

La instalación hay que hacerla a través del paquete *PIP*, que ya viene instalado. Para hacerlo ha hacer lo siguiente:

```tpl
conda activate <nombre entorno>
pip install mglearn
```

Con esto ya se puede usar la librería o paquete.

# Mglearn

## ¿Qué és?

Explicación extráido de [Bioinformatics at COMAV](https://bioinf.comav.upv.es/courses/linux/python/pandas.html)

Pandas es un paquete de Python que proporciona estructuras de datos similares a los dataframes de R. Pandas depende de Numpy, la librería que añade un potente tipo matricial a Python. Los principales tipos de datos que pueden representarse con pandas son:

* Datos tabulares con columnas de tipo heterogéneo con etiquetas en columnas y filas.
* Series temporales.

Pandas proporciona herramientas que permiten:

* leer y escribir datos en diferentes formatos: CSV, Microsoft Excel, bases SQL y formato HDF5
* seleccionar y filtrar de manera sencilla tablas de datos en función de posición, valor o etiquetas
* fusionar y unir datos
* transformar datos aplicando funciones tanto en global como por ventanas
* manipulación de series temporales
* hacer gráficas

En pandas existen tres tipos básicos de objetos todos ellos basados a su vez en Numpy:

* Series (listas, 1D),
* DataFrame (tablas, 2D) y
* Panels (tablas 3D).

## Instalación

Viene incluído en la librería *Numpy*