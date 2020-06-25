---
title: RTTS
description: Runtime Type Services
bookCollapseSection: true
---

# ABAP

El RTTS, Runtime Type Services, es un conjunto de clases que permite la creación de variables, tablas internas de manera dinámica. 

El RTTS es una herramienta tremendamente útil para crear objetos de manera dinámica de una manera bastante sencilla. La jerarquía de clases es la siguiente:

```tpl
CL_ABAP_TYPEDESCR
  |
  |--CL_ABAP_DATADESCR
  |   |
  |   |--CL_ABAP_ELEMDESCR
  |   |--CL_ABAP_REFDESCR
  |   |--CL_ABAP_COMPLEXDESCR
  |       |
  |       |--CL_ABAP_STRUCTDESCR
  |       |--CL_ABAP_TABLEDESCR
  |
  |--CL_ABAP_OBJECTDESCR
     |
     |--CL_ABAP_CLASSDESCR
     |--CL_ABAP_INTFDESCR
```

Aquí se irán poniendo ejemplos diversos sobre el RTTS.

# Publicaciones

{{<section>}}