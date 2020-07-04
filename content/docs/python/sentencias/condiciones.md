---
title: Condiciones
---

# Condiciones

Las condiciones son como en la mayoria de lenguajes

* x == y --> *x es igual a y*
* x != y --> *x es distinto de y*
* x > y --> *x es mayor que y*
* x < y --> *x es menor que y*
* x >= y --> *x es mayor o igual que y*
* x <= y --> *x es menor o igual que y*
* x is y --> *x es lo mismo que y*
* x is not y --> *x no es lo mismo que y*
* Operadores lógicos son: *and(y), or(o) y not(no)*

Para hacer bucles condiciones esta la sentencia *IF*. Ejemplo

Ejemplo sencillo:
```tpl
if 34 == 45:
 print("Son iguales")
else:
 print("No son iguales")
```
Resultado:
```
No son iguales
```

**NOTA: Hay que dejar un espacio en blanco, o una tabulación al escribir la siguiente línea del IF o del ELSE porque sino da un error el interprete. Ya que tiene que estar identado respecto a la sentencia**

Hay una sentencia que es *PASS* que se pondría después del *IF* o del *ELSE* para que no de error. Ejemplo:
```tpl
if 34 <= 45:
 pass
```

No hace nada. Útil cuando todavía no saber que poner. En otros lenguaje se pondría un comentario pero aquí esto da error:

```tpl
if 34 <= 45:
 #no hago nada
```
Da este error::
```
File "", line 2
    #no hago nada
                 ^
SyntaxError: unexpected EOF while parsing
```

También están los *ELSE IF" que se escriben con *ELIF*:
```tpl
if 34 == 45:
    print("Son iguales")
elif 34 > 45:
    print("Es mayor")
else:
    print ("Es menor")
```
Resultado:
```
Es menor
```