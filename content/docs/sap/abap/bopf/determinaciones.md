---
title: Determinaciones
description: Todo lo referente a determinaciones
---

# Objetivo

Las determinaciones se usan para rellenar atributos transitorios, son aquellos campos que se definen en la estructura transitoria. Yo los uso para completar descripciones y hacer cálculos.

Yo suelo usar una clase general para agrupar determinaciones de un mismo nodo. En algunos artículos y el propio BOPF aconseja hacer una clase por determinación. Pero lo veo una manera de generar clases que a lo mejor solo tiene cuatro líneas de código.
En mi caso con una sola clase he tenido suficiente, ya que muchas veces según la complejidad tengo una clase que gestiona dicho proceso.

# Case para ir llamando a las determinaciones

Pongo el case siguiente en el método principal que se llama en la determinación. Y a partir de ese case voy llamando a métodos distintos.

```tpl
 CASE is_ctx-det_key.
      WHEN zif_sat_orders_c=>sc_determination-<nodo>-<nombre determinacion>
ENCASE
```

# Parámetros de un método de la deteminación

El método de la determinación siempre tiene los mismo parámetros que el método estándar, cuya firma es la siguiente:

```tpl
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_det
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
      RAISING
        /bobf/cx_frw.
```