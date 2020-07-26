---
title: Variables y sesiones
description: Variables y sesiones
---

# Introducción

Ejemplo extraído del video [TensorFlow: Variables y Sesiones](https://www.youtube.com/watch?v=XM4HpqTBi5E). 

El ejemplo esta creado el *Jupiter Notebool* y se irá poniendo el código de las distintas celdas.

**NOTA: El código del video esta basado en Tensor Flow 1.0. Pero el Tensor Flow que se ha instalado es la version 2.x, la llamaremos TF2.0 o TF20 . Por lo tanto hay muchas cosas que no son compatibles. En el código habrá partes 
las principales que se indique que no se usa en TF20, pero el resto de partes se pondrá en código TF20 para no poner demasiada basura en el código**

# Código

[Código fuente descargable](/docs/python/machine_learning/tensor_flow/tf_variables_sesiones.ipynb) 

```tpl
# Libería de tensor flow
import tensorflow as tf
# Estas dos líneas es para que funcione el placeholder
#import tensorflow.compat.v1 as tf
#tf.disable_v2_behavior() 
```
```tpl
# Definición de una constante
constante = tf.constant([2.0,3,4], dtype=tf.float32,name='Constante1')
```
```tpl
# Atributos de la constante
print(constante)
```
```tpl
# Para que funcione el placeholder hay que cargar las librerías
# del tensorflow de manera distinta
# Placeholder es una variable que se crea de inicio vacia, pero 
# servira para ir rellenandola más adelante. Normalmente se 
# usan para el "input" de data
# En este ejemplo se usa la versión que sustituye a la antigua
#apartado = tf.placeholder(dtype=tf.float32, name='Variable1') # Version v1
apartado = tf.keras.Input(name='Variable1', shape=(), dtype=tf.float32)
```
```tpl
# Atributos del placeholder
print(apartado)
```
```tpl
# Variables
# Es un atributo que pueda cambiar de valor a lo largo del programa.
variable = tf.Variable(3,dtype=tf.float32,name="variable1")
```
```tpl
# Atributos de variable
print(variable)
```
```tpl
# Se genera un matriz con todo ceros que luego se puede manipular
# Se indica que tendrá 3 filas y 4 columnas 
matriz = tf.zeros([3,4],tf.int32,name='matriz')
```
```tpl
# Atributo
print(matriz)
```
```tpl
# Se inicializa las variables de nuestra aplicación
# Esto con TF2.0 ya no es necesario
#inicializar = tf.global_variables_initializer()
```
```tpl
# Inicializa la sesión de tensorflow. Con TF2.0 no es necesario
#sess = tf.Session()
```
```tpl
# Lo que hace es informar nuestras variables en las sesion de TF
# Este paso no es necesario usarlo en TF2.ProcessLookupError0
#sess.run(tf.global_variables_initializer())
```
```tpl
# No hace usar sar sess.run para cualquier cosa en TF20
#print(sess.run(constante))
```
```tpl
# Se hace la multiplicacion entre apartado y constante
multiplicacion=apartado*constante
```
```tpl
# No habrá resultado porque apartado esta vacio
print("Valor de multiplicacion:", multiplicacion)
```
```tpl
# Se rellena los valores de apartado
apartado = [[15,10,5]]
```
```tpl
# Ahora si que devuelve datos. Cada columna se multipla por la constante 
# que es: 2.0, 3 y 4 * 15, 10 y 5. Queda como resultado 30, 30 y 20
print("Valor de multiplicacion:", multiplicacion)
```
```tpl
# Ejemplo de multiplicación de matrices necesita la forma (M,N)X(N,M)=M,M
# Primera matriz: 2 filas y 2 columnas
a = tf.keras.Input(shape=(2,2), dtype=tf.float32)
# Segunda matriz: 2 filas y 3 columnas
b = tf.keras.Input(shape=(2,3), dtype=tf.float32)
```
```tpl
# Datos de la primera matriz
a = [[1,2],[2,2]]
# Datos de la segunda matriz
b = [[12,21,4],[3,2,4]]
```
```tpl
# Multiplicación de ambas matrices: (2,2) * (2,3)
mult = tf.matmul(a,b)
```
```tpl
# Resultado
print("Resultado de mult: ", mult)
```
```tpl
# Ejemplo de producto . entre dos vectores
# Primer vector: 3 columnas
c = tf.keras.Input(shape=(3), dtype=tf.float32)
# Segundo vector: 3 columnas
d = tf.keras.Input(shape=(3), dtype=tf.float32)
```
```tpl
# Datos para los vectores
# Primer vector
c = [1,2,3]
# Segundo vector
d = [3,2,1]
```
```tpl
# Calculo del producto punto
punto = tf.tensordot(c,d,1)
```
```tpl
print("Resultado: ", punto)
```