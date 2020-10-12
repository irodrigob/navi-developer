---
title: Diccionario
description: Diccionario
---

# Introducción

Son como las listas o tuplas pero al acceso a sus valores no se accede por un índice, sino que se accede por la clave asociada al valor. 

Por lo que he visto hay diccionarios simples que es una relación clave<->valor y otros más complejos. De momento explicaré los simples para tener una base

## Como saber si un variables es tipo dict

Hay dos maneras:
```tpl
if type(<variable>) is dict 
```

```tpl
isinstance(<variable>, dict)
```

## Simples

Crear un diccionario es muy simple tan sólo hay que usar la siguiente sentencia:
```tpl
eng2sp = dict()
```

### Añadir valores
 
#### Método simple

```tpl
eng2sp['one'] = 'uno'
print(eng2sp)
```
Resultado:
```
{'one': 'uno'}
```
Con múltiples valores:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
print(eng2sp)
```
Resultado:
```
{'one': 'uno', 'two': 'dos', 'three': 'tres'}
```
Para ver como se llenaría dinámicamente vamos a contar las letras de una palabra:
```tpl
palabra = 'ejemplo'
d = dict()
for c in palabra:
    if c not in d:
        d[c] = 1
    else:
        d[c] = d[c] + 1
print(d)
```
Resultado:
```tpl
{'e': 2, 'j': 1, 'm': 1, 'p': 1, 'l': 1, 'o': 1}
```

#### Usando *setdefault*

El *setdefault* es una propiedad que permite añadir una clave con un valor por defecto en caso de no existir. Es ideal para inicializar un diccionario a partir de una lista:

```tpl
block_num_index = {}
for index, value in enumerate(data['block_num']):
    block_num_index.setdefault(value,[]).append(index)    
```

### Obtener valores
Para obtener valores se puede acceder directamente por índice:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
print(eng2sp['two'])
```
Resultado
```
dos
```
O con la método *get* que es implicito del objeto *dict*
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
print(eng2sp.get('two'))
```
Resultado
```
dos
```
Si se informa un valor que no existe, ejemplo:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
print(eng2sp.get('four'))
```
El resultado es:
```
None
```
Si queremos que devuelva un valor en caso de no existe podemos hacer lo siguiente:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
print(eng2sp.get('four',0))
```
El resultado es:
```
0
```
### Buscar datos

Para buscar datos podemos usar el operador *in* pero este operador nos dirá si existe, o no, una clave. Ejemplo:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
if 'one' in eng2sp:
    print("existe")
else:
    print("No existe")
```
Resultado:
```
existe
```
Si se quiere buscar por valores primero hay que recuperar una lista:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
valores = list(eng2sp.values())
print(valores)
```
Resultado:
```
['uno', 'dos', 'tres']
```
Hay que usar la sentencia *list* que nos convierte la lista de tipo *dict* a una lista que se puede usar el *in* para buscar.


### Recorrer los datos
Un *dict* se puede recorre usando un *for* pero hay que recordar que nos irá devolviendo la clave:
```tpl
eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
for clave in eng2sp:
    print(f"Clave: {clave} - Valor: {eng2sp.get(clave)}")
```
Resultado:
```tpl
Clave: one - Valor: uno
Clave: two - Valor: dos
Clave: three - Valor: tres
```

## Itertools

Python tiene una librería llamada [Itertools](https://docs.python.org/3/library/itertools.html) que simplifica las operaciones con diccionarios. Para poderlas utilizar tan solo hay que declararlas en nuestro código de la siguiente manerA:

```tpl
import itertools as it
```
En el ejemplo le pongo un alias para que simplificar sus llamadas.