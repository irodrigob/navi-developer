---
title: Atajos
description: Código para ser usado 
weight: 20
---

# Objetivo

Disponer de pequeños trozos de código que al no usarse mucho me cuestan saber como utilizarlo y siempre tengo que estar buscandolos.

# Parámetros

```tpl
define view ZATRON_CDS_DOMAIN_TEXTS
  with parameters
    p_domain :DOMNAME,
```

# Valores por defecto

En este ejemplo se usa para pasar por defecto en un parámetro de entrada. Pero la misma variable variable de entorno se puede usar en un *WHERE*
```tpl
 @Environment.systemField: #SYSTEM_DATE
    p_langu  :abap.dats
```