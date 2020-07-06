---
title: Filtros
description: Sentencias sobre el filtrado de datos
bookToc: true
---

# Introducción

En este grupo se irán poniendo distintos ejemplos de la sentencia *FILTER* que es un sentencia que no he usado mucho.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

# Ejemplo 1

```tpl
<ls_values_search_sap>-kotab_data = FILTER #( lt_values IN FILTER #( lt_konp USING KEY loevm WHERE loevm_ko = '' ) WHERE knumh = knumh ).
```

Lo bueno del filter es que te crea una tabla o te mueve directamente el contenido de la tabla con el filtro aplicado. En el caso de usar búsqueda directa (segundo FILTER) te obliga, eso creo, ha tener una clave para la búsqueda.

# Ejemplo 2

```tpl
data(et_cust_hier) = FILTER #( lt_cust_hier IN lt_cust_sales USING KEY kunnr WHERE kunnr = kunnr ).
```

Se filtra el contenido de la tabla LT_CUST_HIER con los valores en LT_CUST_SALES filtrando por el cliente. Importante la tabla LT_CUST_SALES debe ser de tipo SORT.
ET_CUST_HIER se creará del mismo tipo que LT_CUST_HIER.