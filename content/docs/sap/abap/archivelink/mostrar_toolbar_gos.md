---
title: Mostrar la barra de herramientas GOS
description: Mostrar la barra de herramientas GOS
---

# Objetivo

Cuando le damos al botón del GOS en una transacción estándar nos sale una barra de herramientas donde podemos ver, borrar, consultar, etc.. los documentos. Para mostrar dicha barra de herramient en un programa nuestro tenemos que hacer lo siguiente:

```tpl
 DATA:
      lr_gos    TYPE REF TO cl_gos_manager,
      ls_object TYPE borident.


    ls_object-objkey = gs_selected_opm-header-posnr.
    ls_object-objtype = 'BUS2104'.

    CASE gr_app->mode.
      WHEN zcl_coim_opm_tools=>c_mode_edit.
        CREATE OBJECT lr_gos
          EXPORTING
            ip_no_commit = abap_false
            ip_mode      = 'E'.
      WHEN zcl_coim_opm_tools=>c_mode_view.
        CREATE OBJECT lr_gos
          EXPORTING
            ip_no_commit = abap_false
            ip_mode      = 'D'.
    ENDCASE.

    lr_gos->display_toolbox(
      EXPORTING is_object = ls_object ).
```