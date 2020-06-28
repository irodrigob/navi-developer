---
title: ALV
description: ALV
bookCollapseSection: true
---

# Objetivo

Cajón de desastre de cosas interesante de los ALV. No descarto que alguna cosa este duplicada en alguna otra sección como clases o funciones.


# Proyectos personales

En el siguiente [enlace](https://github.com/irodrigob/alv_utilities) hay una clase que permite simplificar la creación de ALV. Esta clase encapsula las clases de los SALV.

# Mostrar un ALV como si fuera una dynpro pero sin dynpro

* CL_GUI_CONTAINER=>SCREEN0 --> Permite sacar un ALV como si fuera dynpro si tener una o configurar container. En los nuevos SALV hay que poner después del display un WRITE: 'X'.

# Recuperar contenido mostrado por un ALV

```tpl
cl_salv_bs_runtime_info=>set(
       EXPORTING display  = abap_false
                 metadata = abap_true
                 data     = abap_true ).
```

Para evitar que un ALV se muestre y asi poder recoger su catalogo,filtros, etc.. . Eso si, el truco del SCREEN0 provoca que no funcione el DISPLAY con este truco