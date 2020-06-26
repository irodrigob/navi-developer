---
title: Funciones ventas
description: Funciones ventas
bookCollapseSection: true
---

# Objetivo

Funciones de ventas

# Lista

Función | Descripción | Ejemplo
--------|-------- | --------
RV_DOCUMENT_FLOW | Sacar el flujo de documentos en ventas | 
```tpl
FORM ir_flujo  USING pe_datos TYPE LINE OF zzc_sd49501=>ty_t_alv.
  DATA ls_vbco6 TYPE vbco6.

  MOVE-CORRESPONDING pe_datos TO ls_vbco6.

  CALL DIALOG 'RV_DOCUMENT_FLOW'
    EXPORTING
      vbco6      FROM ls_vbco6
      makt-maktx FROM space
      kna1-kunnr FROM pe_datos-kunnr
      kna1-name1 FROM pe_datos-txt_kunnr
      makt-matnr FROM space
      ivkorg     FROM pe_datos-vkorg
      ivtweg     FROM pe_datos-vtweg.

ENDFORM.                    " IR_FLUJO
```
