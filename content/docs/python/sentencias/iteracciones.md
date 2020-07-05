---
title: Iteracciones
---

# Iteracciones

## Iteracciones condicionales

Llamo iteracciones condicionales aquellas sentencias que no recorren listas sino simplemente repetin un bloque de sentencias hasta que se cumple una condición. La sentencia *WHILE* sería un ejemplo de ello:

```tpl
n = 5
print("Contador")
while n >= 0:
    print(n)
    n = n - 1
```
Resultado:

```
Contador
5
4
3
2
1
0
```

Con la sentencia *BREAK* se puede salir de un bucle. Ejemplo:
```tpl
n = 5
print("Contador")
while True:
    print(n)
    n = n - 1
    if n == 0:
        break
```
Resultado:
```
Contador
5
4
3
2
1
```

También se tiene la sentencia *CONTINUE*. Esta sentencia salta a la siguiente interacción. Ejemplo:
```tpl
n = 6
print("Contador")
while n >= 0:
    n = n - 1    
    if ( n % 2 ) != 0: # Solo se pintan los pares
        continue
    print(n)
```
Resultado:
```
4
2
0
```
**NOTA PERSONAL: Esta sentencia no la uso en otros lenguajes menos en Python. Pero la pongo para saber que existe.**

## Iteracciones listas

Aquí encontramos la sentencia *FOR* que permite recuperar una lista de valores y procesarlas. Ejemplo:

```tpl
numeros = [5,4,3,2]
for numero in numeros:
    print("Número: ", numero)
```
Resultado:
```
Número:  5
Número:  4
Número:  3
Número:  2
```
