---
title: Acciones
description: Todo lo referente a las acciones
---

# Objetivo

Las acciones se usan para llamar a procesos externos del BOPF. Por ejemplo yo los uso para crear un pedido, lanzar otro programa, etc..

Yo suelo usar una clase general para agrupar acciones de un mismo nodo. En algunos artículos y el propio BOPF aconseja hacer una clase por acción. Pero lo veo una manera de generar clases que a lo mejor solo tiene cuatro líneas de código.
En mi caso con una sola clase he tenido suficiente, ya que muchas veces según la complejidad tengo una clase que gestiona dicho proceso.

# Case para ir llamando a las determinaciones

Pongo el case siguiente en el método principal que se llama en la acción. Y a partir de ese case voy llamando a métodos distintos.

```tpl
CASE is_ctx-act_key.
      WHEN zif_sat_orders_c=>sc_action-<nodo>-<nombre determinacion>
ENCASE

```

# Parámetros de un método de la deteminación

El método de la determinación siempre tiene los mismo parámetros que el método estándar, cuya firma es la siguiente:

```tpl
IMPORTING
        !is_ctx                  TYPE /bobf/s_frw_ctx_act
        !it_key                  TYPE /bobf/t_frw_key
        !io_read                 TYPE REF TO /bobf/if_frw_read
        !io_modify               TYPE REF TO /bobf/if_frw_modify
        !is_parameters           TYPE REF TO data
      EXPORTING
        !eo_message              TYPE REF TO /bobf/if_frw_message
        !et_failed_key           TYPE /bobf/t_frw_key
        !ev_static_action_failed TYPE abap_bool
        !et_data                 TYPE INDEX TABLE
      RAISING
        /bobf/cx_frw .
```