---
title: Cadenas
description: Cadenas
---

# Cadenas

Las cadenas son secuencias de carácteres. Es decir, una variable de tipo de string puedes acceder a una posición concreta. Es como acceder a una posición de un array o lista. Ejemplo:


```tpl
texto = "hola"
print("Segunda letra del texto es:", texto[1])
```
Resultado:
```
Segunda letra del texto es: o
```
**OJO: Que aquí los arrays también empiezan por 0.**

Con la sentencia *LEN* se puede saber el tamaño de la cadena. Ejemplo:

```tpl
texto = "hola"
print("Primera letra del texto es: ", texto[0])
longitud = len(texto)
print("Última letra del texto es: ",texto[longitud-1])
```
Resultado:
```
Primera letra del texto es:  h
Última letra del texto es:  a
```
Como en Javascript la longitud hay que restarle uno para poder acceder al último elemento, o para hacer bucles.

Hay que tener en cuenta que las cadenas no se puede modificar como si se puede hacer en otros lenguajes, solo podemos consultarla. Es decir, esto:

```tpl
texto="hola"
texto[2]="C"
```
Da el siguiente error:
```
TypeError                                 Traceback (most recent call last)
 in 
      1 texto="hola"
----> 2 texto[2]="C"

TypeError: 'str' object does not support item assignment
```

## Bucles 
A las cadenas también se puede acceder haciendo iteracciones. Ejemplo:

Ejemplo:
```tpl
texto="hola"
n=0
while n < len(texto):
    print("Letra: ",texto[n])
    n = n + 1
```
Resultado:
```
Letra:  h
Letra:  o
Letra:  l
Letra:  a
```
El ejemplo anterior pero usando *FOR*
```tpl
texto="hola"
for letra in texto:
    print("Letra: ", letra)
```
Dando el mismo resultado.

## Accediendo a las partes de la cadena

También se puede acceder a trozos de una cadena:

```tpl
texto="hola mundo"
print("Primera parte: ", texto[0:4])
print("Segunda parte: ", texto[5:10])
```
Resultado:
```
Primera parte:  hola
Segunda parte:  mundo
```

## Operadores en condiciones

El operador *IN* se menciona en la página de [condiciones](/docs/python/sentencias/condiciones.md)  pero pongo el ejemplo aquí que es su sitio:

```tpl
texto="hola"
if "o" in texto:
    print("Existe la O")
if "j" in texto:
    pass
else:
    print("No existe la J")  
```
Resultado:
```
Existe la O
No existe la J
```

También se puede usar los *<* o *>* para hacer comparaciones por orden alfabético.

## Métodos de cadenas

Las cadenas son también objetos que por defecto tienen su propios métodos. Para saber que métodos tiene un cadena hay que hacer lo siguiente:

```tpl
texto="hola"
dir(texto)
```
El resultado no lo pongo porque es muy largo y supongo que los métodos variarán segun la versión de Python usada.
Un ejemplo de como se usaría:

```tpl
texto="hola"
print("texto en minúsculas", texto)
print("texto en mayñusculas", texto.upper())
```
Resultado:
```
texto en minúsculas hola
texto en mayñusculas HOLA
```

Para saber como funciona mejor es irse a la [docu oficial de Python](https://docs.python.org/3/library/stdtypes.html#string-methods).

## Formatos

Como cadenas se les puede aplicar formatos. El carácters para aplicar formatos es *%* y a continuación el tipo de valor que se va formtear:

* d --> Decimal
* g --> Flotante
* s --> String

Ejemplo:

```tpl
texto="ivan"
print("hola %s" % texto)
```
Resultado:
```
hola ivan
```

Se puede sustituir varios valores pero hay que definir una tupla. Ejemplo:
```tpl
print("hola %s, hoy es dia %d y son las %g" % ("ivan", 5, 16.32))
```
Resultado:
```
hola ivan, hoy es dia 5 y son las 16.32
```
El numero de formatos en la cadena debe coincidir con el número de valores en una tupla para que no de error.


## String sin carácteres de escape

Si queremos inicializar una variable con un ruta de nuestro disco duro como esto:
```tpl
path = "c:\archivos de programa\test\test.exe" 
```
No se almacene bien porque python por defecto usa el carácter *\* como carácter de escape. Si queremos decirle que lo trate como un *raw string*, es decir, literal sin tranformaciones hay que poner una *r* delante del literal, tal que así:
```tpl
path = r"c:\archivos de programa\test\test.exe" 
```

