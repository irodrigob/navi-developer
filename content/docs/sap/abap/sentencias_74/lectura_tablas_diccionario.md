---
title: Lectura tablas del diccionario
description: Formas nuevas de leer en las tablas del diccionario
---

# Introducción

Realmente no tenemos sentencias específica sino que es ampliación del Open SQL para dotarle de más flexibilidad a la hora de leer información

# Ejemplo 1

Con este select extendido se busca una parte de los campos de una tabla, pero el resto de campo se completan segun variables rellenadas previamente.

```tpl
    SELECT  kschl, kotab, @is_header-customer_code AS keyvalue, @is_header-validity_to AS datab, 
            @is_header-validity_from AS datbi, @zif_rtn_data=>cv_updkz_insert AS updkz
            INTO CORRESPONDING FIELDS OF TABLE @et_cond_crud
            FROM ztippqps
            WHERE pqtyp = @is_header-negotiation_type_code
                  AND keyfield = @lv_keyfield.
```

Tenemos que pondemos rellenar campos de una tabla/estructura a partir de una constantes. Antes se tendría que haber hecho un *loop* para hacerlo. Cuando usamos estas capacidades extendidas hay que usar el símbolo *@* en las variables que no pertencen al selecy: parámetros, variables, etc..

# Ejemplo 2

Mismo select que el del ejemplo 1 pero crea la tabla interna donde se guardarán los datos de manera automática, sin declarar previamente.

```tpl
    SELECT  kschl, kotab, @is_header-customer_code AS keyvalue, @is_header-validity_to AS datab, 
            @is_header-validity_from AS datbi, @zif_rtn_data=>cv_updkz_insert AS updkz
            INTO TABLE @data(et_cond_crud)
            FROM ztippqps
            WHERE pqtyp = @is_header-negotiation_type_code
                  AND keyfield = @lv_keyfield.
```