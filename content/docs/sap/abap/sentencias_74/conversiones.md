---
title: Conversiones
description: Sentencias sobre conversión de datos
---

# Introducción

En este grupo se irán poniendo las distintas sentencias que nos permiten convertir valores de un campos a otro.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

# Ejemplo 1

Lo que antes se hacia:

```tpl

DATA lo_elemdesc type ref to cl_abap_elemdescr.
Lo_elemdesc ?= cl_abap_typedescr=>describe_by_data( iv_abap_value.

Ahora se puede hacer en una sola línea:

DATA(lo_elemdesc) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( iv_abap_value ) ).
```

# Ejemplo 2

Convertir los valores de un select para adaptarlos a la variable donde se van a guardar:

```tpl
    SELECT SINGLE CAST( months_past AS INT2 ), CAST( months_future AS INT2 ) INTO (@ev_months_past, @ev_months_future)
           FROM zrtn_t_0037
           WHERE vkorg = @iv_vkorg
                 AND vtweg = @iv_vtweg
                 AND spart = @iv_spart.
```