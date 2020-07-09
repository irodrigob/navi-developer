---
title: Usar liberias
---

# Usar librerias

Importar libreria se hace con la sentencia *Import* pero tiene variantes según sean librerias propias de Python o sencillas, eso creo, o librerias que están dentro de un paquete.

## Librerías propias o sencillas

Ejemplo de como usar la libreria matemática para calcular una raíz cuadrada:

```tpl
import math

print(math.sqrt(20)) 
```
Resultado
```
4.47213595499958
```

## Librerías dentro de paquetes

El siguiente ejemplo se recupera la libreria de regresión de KNN:
```tpl
# Regresor de KNN o vecinos cercanos
from sklearn.neighbors import KNeighborsRegressor
```

