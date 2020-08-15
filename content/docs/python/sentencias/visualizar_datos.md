---
title: Visualizar datos
description: Visualizar datos
---

# Visualizar datos

En todos los ejemplos se hace uso de la sentencia *print* a la hora de visualizar los datos. En esta página se recopila usos que se hace de ella y otras posibles formar de visualizar en pantalla

# *print*

El *print* muestra los datos en la consola doned se esta ejecutando el programa. Aquí se recopilan formas de usarlo

## Hacer operaciones

Es posible mostrar el resultado de una operación que se pone directamente en la sentencia:
```tpl
h=12
a=12
print("Suma: ",a+h)
```
Resultado
```
Suma:  24
```

## Mostrar contenido de variables

Opción más simple:
```tpl
n=20
print("Valor de n: ",n)
```
Resultado:
```
Valor de n: 10
```
O también poner directamente el valor del variables:
```tpl
n=20
print(n)
```
Resultado:
```
10
```

Pero la **opción que más me gusta** por ser la más simple es estA::
```tpl
n=20
print(f"Valor de n: {n}")
```
Resultado:
```
Valor de n: 10
```

Tan solo hay que poner el carácter *f* delante de las comillas dobles o simples, al gusto, y poner entre los carácteres *{}* la variable.



