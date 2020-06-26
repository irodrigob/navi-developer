---
title: Código ejemplo dentro del BOPF
description: Código ejemplo dentro del BOPF
---

# Objetivo

Aquí están las plantillas de ejemplo ABAP que se usarán dentro de las clases que se usarán en validaciones, determinaciones, etc..

# Instanciar clase de mensajes

Los mensajes de los BOPF se recogen en una clase generica que pueda ser usada en las determinaciones, validaciones, etc..

```tpl
    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.
```

# Template código usado en acciones, validaciones y determinaciones

En cada método de una determinación, validación o acciones siempre hay los siguientes pasos, más o menos:

1. Leer los datos según la clave que se recibe por párametro. Este paso siempre hay que hacerlo da igual lo que se haga
2. Procesar los datos
3. Actualizar los datos en el nodo. Este paso es opcional según lo que estemos haciendo

## Lectura de datos

```tpl
DATA lt_data TYPE zatron_bo_i_file_eng_header.

    " Lectura de datos
    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).
```

## Actualización

```tpl
LOOP AT lt_data REFERENCE INTO DATA(lo_data).

      " Actualización  
      io_modify->update( iv_node = is_ctx-node_key
                   iv_key  = lo_data->key
                   is_data = lo_data ).
    ENDLOOP.
```

