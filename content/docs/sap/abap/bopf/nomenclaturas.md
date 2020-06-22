---
title: Nomenclaturas
description: Nomenclaturas
weight: 20
---

# Objetivo

Es tener las indicaciones de las nomenclaturas que uso cuando creo un BOPF. Ya que de esta manera es fácil identificar los objetos a simple vista.

Yo normalmente cuando hago proyecto suele dar un nombre de tres carácteres que usará como prefijo para cualquier objeto que creo, ya sea, BOPF, ya sea, cualquier otro objeto: clases, transacciones, etc.

# Estructuras y tablas de diccionario

En la siguiente imagen:

![Ejemplo](/images/sap/abap/bopf/ejemplo_nomenclatura_estructuras_tablas.png)

Es un ejemplo de la nomencltura utilizada en un proyecto real. **NOTA: A día de hoy hay una ligera variación, cambia el nombre de la estructura persistente** 

El esquema sería el siguiente:

* La estructura para los persistentes sería --> ```Z<nombre proyecto>_BO_SP_<libre>```
* La estructura de los datos transitorios --> ```Z<nombre proyecto>_BO_ST_<libre>```
* La estructura que combina las dos estructura anteriores  --> ```Z<nombre proyecto>_BO_SC_<libre>```
* El tipo de tabla para la estructura combinada --> ```Z<nombre proyecto>_BO_I_<libre>```
* Tabla de base de datos --> ```Z<nombre proyecto>_T_<libre>```

# Determinaciones

Sería: ```ZCL_<nombre proyecto>_D_<nombre bo>_<nodo>```

Ejemplo: ZCL_ATRON_D_FILE_ENG_HEADER

# Validaciones

Sería: ```ZCL_<nombre proyecto>_V_<nombre bo>_<nodo>```

Ejemplo: ZCL_ATRON_V_FILE_ENG_HEADER

# Acciones

## Clase
Sería: ```ZCL_<nombre proyecto>_A_<nombre bo>_<nodo>```

Ejemplo: ZCL_ATRON_A_FILE_ENG_HEADER

## Estructura para los parámetros y tipo de datos para la salida de datos

### Estructura de parámetros

Sería: ```Z<nombre proyecto>_BO_SA_PARAMS_<nombre accion>>```

El *SA* sería "Structure Action"

Ejemplo: ZATRON_BO_SA_PARAMS_LAUNCH_ETL

### Estructura para la salida de datos

Sería: ```Z<nombre proyecto>_BO_SA_EXPORT_<nombre accion>>```

Ejemplo: ZATRON_BO_SA_EXPORT_LAUNCH_ETL

### Tipo tabla para la salida de datos

Sería: ```Z<nombre proyecto>_BO_IA_EXPORT_<nombre accion>>```

Ejemplo: ZATRON_BO_IA_EXPORT_LAUNCH_ETL

# Querys

Sería: ```ZCL_<nombre proyecto>_Q_<nombre bo>_<nodo>```

Ejemplo: ZCL_ATRON_Q_FILE_ENG_HEADER
