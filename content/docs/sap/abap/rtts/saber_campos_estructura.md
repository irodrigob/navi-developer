---
title: API desde ABAP
description: Como usar la API de los BOPF desde ABAP
---

# Ejemplo

```tpl
DATA(lt_components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( cs_row_data ) )->get_components(  ).
```

