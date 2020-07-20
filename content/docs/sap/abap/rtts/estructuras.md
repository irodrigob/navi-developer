---
title: Estructuras
description: Estructuras
---

# Introducción

Que se puede hacer con estructuras ya sean del diccionario como tipos de datos estructurados creados dentro del código.

# Ejemplo

Manera simple de saber los campos de una estructura. Dentro de los campos de la tabla devuelta esta el campo con el tipo de datos del campo. De él se puede obtener la info técnica de dicho campos.

```tpl
DATA(lt_components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( cs_row_data ) )->get_components(  ).
```

