---
title: Sentencias
description: Sentencias
bookCollapseSection: true
---

# Introducción

Sentencias propias del lenguaje.

Como nota estoy usando para hacer las pruebas el Jupyter Notebook porque directamente pones el código y pulsando *CTRL+ENTER* se ejecuta y lo ves justo debajo.

# Comentarios

Los comentarios se ponen con el carácter *#*

# Uso del ":" para sentencia con bloques de código

En las sentencias que tienen dentro de ellas bloques de código: *IF*, *WHILE*, *FOR*,etc, hay que poner el carácter ":" al final de la sentencia. Ejemplo:

```tpl
if condicion 1 == condicion2: 

while n < 10:
```


# Código dentro de sentencias

A diferencia de otros lenguajes que usán, o bien, carácteres especiales (como el *{}* como en Javascript), o bien, sentencias de inicio y fin para identificar el código que hay dentro de una sentencia: IF, FOR, etc.. En Python eso se hace
identado, ya sea con un espacio en blanco o tabulacion, para indicar que las líneas de código pertenecen a una sentencia. Por ejemplo:

```tpl
if a <> b:
    sentencia 1 del IF
    sentencia 2 del IF

setencia fuera del IF
```

Si no identamos entonces va a dar un mensaje de error porque se va pensar que la sentencia, en este caso el IF, no tiene código.

# Visualizar datos

Con el comando *print* podemos visualizar el contenido de variables    

```tpl
print(a)
print("El valor de a es:",a)
```
Resultado:
```
12
El valor de a es: 12
```

# Secciones

El detalle de sentencias más especificamente:

{{<section>}}