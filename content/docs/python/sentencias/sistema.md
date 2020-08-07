---
title: Sistema
description: Sistema
---

# Introducción

La librería *os* hay una serie de funciones de sistema que se pueden utilizar. Aquí se recopilan las que se van usando. Esta librería para utilizar hay que hacer lo siguiente al inicio del programa:

```tpl
import os
```

# Logs

Cuando se usán librerías de machine learning como *Tensor Flow* en programa de Python, si se usa *Jupyter Notebooks* se muestran una cantidad de logs, cientos de líneas, que desvirtuan lo que realmente se quiere mostrar.

Para eso se puede usar la siguiente sentencia:

```tpl
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  
```

Esto reduce los logs que se producen al importar librerias o informativos del propio Python. Esto no elimina los mensajes que se generen a proposito en sentencias de las librerías, para eso la propia librería tendrá, o no, su propias sentencias.

La tabla con los valores posibles es la siguiente:

Nivel | Nivel entendible | Descripción del nivel
--------|--------|--------
0 | DEBUG | [Default] Print all messages       
1 | INFO | Filter out INFO messages           
2 | WARNING | Filter out INFO & WARNING messages 
3 | ERROR | Filter out all messages   

# Directorios

Sentencia que trabajan con directorios

## Validar que exista un directorio

```tpl
if os.path.exists(target_dir):
```

## Crear un directorio
```tpl
os.mkdir(target_dir)
```

