---
title: Captura de excepciones
---

# Captura excepciones

Para captura excepciones y que no salgan error raros al hacer operaciones tenemos la sentencia  *TRY..EXCEPT*

En el siguiente ejemplo extráido de un manual hay una formula para pasar de grados Farenheit a Celsius:

```tpl
ent = input('Introduzca la Temperatura Fahrenheit:')
try:
 fahr = float(ent)
 cel = (fahr - 32.0) * 5.0 / 9.0
 print("Grados celsius":cel)
except:
 print('Por favor, introduzca un número')
```
**NOTA: Aquí también tiene que haber una identación en el código dentro del TRY y del EXCEPT**

Resultado:
```
Grados celsius:  7.222222222222222
```

Si se pone un valor no válido, como un string:
Resultado:
```
Por favor, introduzca un número
```