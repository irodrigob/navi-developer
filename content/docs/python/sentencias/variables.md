---
title: Variables
description: Variables
weight: 20
---

# Variables

Existen tres tipos de variables, al menos por lo que he visto que son: string, enteras y flotantes.

```tpl
# Esta es integer
a=23
# Esta es float
b=23.456
# Esta es string
c="hola mundo"
```

Para saber el tipo de variable en cualquier momento se puede usar la siguiente sentencia:
```tpl
type(c)
```

# Convertir variables

Si queremos convertir el formato de una variable a otra. Como hay tres tipos de "tipos", hay tres funciones que lo hace: *INT*, *FLOAT*, *STR*

Tenemos este ejemplo:
```tpl
h="12"
print("Suma: ",a+h)
```
Al ejecutarlo nos dará esto:
```
TypeError: unsupported operand type(s) for +: 'int' and 'str'
```
Pero si hacemos cambiamos el código:
```tpl
h="12"
print("Suma: ",a+int(h))
```
Al ejecutarlo nos dará esto:
```
Suma:  24
```

No hay que decir que para poderlo el ejemplo anterior en la variable *h* tiene que haber un número. Si tuviese el valor *12r* daría un error porque hay un carácter que no es númerico.

# Borrar contenido de variables

Para borrar el contenido de una variable se usa la sentencia *DEL* ejemplo:

```tpl
del KNN
```