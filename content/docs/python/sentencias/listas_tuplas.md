---
title: Listas y tuplas
description: Listas y tuplas
---

# Listas y tuplas

Las listas son las que usan [] y las tuplas(). Ambas sirven para guardar datos. Y ambas están indexadas por 0, es decir, al primer registro se accede por el valor 0.

Los string en si mismos son listas como se explica en pagína de [cadenas](/docs/python/sentencias/cadenas.md). Pero mientras que en la cadenas los valores son carácteres en una lista/tupla puede contener cualquier valor.

Ejemplo de inicialización:

```tpl
tupla=(1,2,3,4,5,6,7)
lista=[8,9,10,11,12]
```

La diferencia en que la lista no puede ser modificada durante el programa pero una lista si que se puede. Salvo en las cadenas, que no pueden ser modificadas.

Un truco para obtener el último valor sin usar el *LEN* es usar el *-1*. Ejemplo:
```tpl
tupla=(1,2,3,4,5,6,7)
lista=[8,9,10,11,12]

print("Último valor: ", lista[-1])
print("Penúltimo valor: ", lista[-2])
```
Resultado:
```
Último valor:  12
Penúltimo valor:  11
```

## Lista

### Modificar valores
Para cambiar una lista es tan sencillo al índice y poner un nuevo valor. Ejemplo:
```tpl
lista=[8,9,10,11,12]

print("Valores antes del cambio:", lista)
# Cambiamos el segundo registro
lista[1]=15
print("Valores después del cambio:", lista)
```
Resultado:
```
Valores antes del cambio: [8, 9, 10, 11, 12]
Valores después del cambio: [8, 15, 10, 11, 12]
```

### Borrar valores

Para modificar valores se usa el método interno de la lista llamado *POP*. Ejemplo como borrar el último valor:

```tpl
lista=[8,9,10,11,12]

print("Valores antes:", lista)
lista.pop()
print("Valores después:", lista)
```
Resultado:
```
Valores antes: [8, 9, 10, 11, 12]
Valores después: [8, 9, 10, 11]
```

Para borrar una posición concreta se puede hacer de dos maneras con el método *POP* o con la sentencia *DEL*. Ejemplo con *POP*:

```tpl
lista=[8,9,10,11,12]

print("Valores antes:", lista)
lista.pop(1)
print("Valores después:", lista)
```
Resultado:
```
Valores antes: [8, 9, 10, 11, 12]
Valores después: [8, 10, 11, 12]
```
Ejemplo con *DEL*
```tpl
lista=[8,9,10,11,12]

print("Valores antes:", lista)
del lista[1]
print("Valores después:", lista)
```
Resultado:
```
Valores antes: [8, 9, 10, 11, 12]
Valores después: [8, 10, 11, 12]
```
En ambos casos hay que tener en cuenta que el índice empieza por 0.

### Añadir valores

Para añadir se usa el método *APPEND* de la lista. Ejemplo:
```tpl
lista=[8,9,10,11,12]

print("Valores antes:", lista)
lista.append(13)
print("Valores después:", lista)
```
Resultado:
```
Valores antes: [8, 9, 10, 11, 12]
Valores después: [8, 9, 10, 11, 12, 13]
```

### Sublistas

Es posible tener una sublista dentro de una lista ejemplo:

```tpl
lista=["valor 1",2, 3.4,["subvalor", 10]]
print("Contenido ", lista)
```
Resultado:
```
Contenido  ['valor 1', 2, 3.4, ['subvalor', 10]]
```

### Segmentos

Igual que en las cadenas también se puede acceder a segmentos. Ejemplo:
```tpl
lista=[1,2,3,4,5,6]
print("Contenido ", lista[3:5])
```
Resultado:
```
Contenido  [4, 5]
```

Si no se pone el primer valor del segmento se comienza a leer desde el primer valor. Ejemplo:
```tpl
lista=[1,2,3,4,5,6]
print("Contenido ", lista[:5])
```
Resultado:
```
Contenido  [1, 2, 3, 4, 5]
```


## Tuplas

El acceso a los valores de las tuplas se hace igual que con las listas.  