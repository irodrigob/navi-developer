---
title: Ficheros
description: Ficheros
---

# Ficheros

## Leer un fichero

Se puede hacer de dos manera con la sentencia *WITH* y sin ella. 

Ejemplo modo sin *WITH*

```tpl
file = open("welcome.txt")

data = file.read()

print data

file.close() # Importante hay que cerrar el fichero para evitar bloqueos
```

Con la opción *WITH*

```tpl
with open("hola.txt") as file: 
   data = file.read()
```
Con *WITH* parece ser que el control de excepciones es mejor y tareas simples como la preparación del fichero y su cierre se hace de manera automática. Con lo cual evita errores tontos en el tratamiento de ficheros.

Si queremos abrir el fichero para escritura con *WITH* sería:

```tpl
with open('salida.txt', 'w') as file:
    file.write('hola!')
```


