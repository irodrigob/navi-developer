---
title: Ventas
description: Ventas
---

# Objetivo

Funciones del módulo de ventas(SD). 

Al final hay una sección de ejemplos para algunas funciones. **No todas las funciones tienen ejemplo.**

# Lista

Función | Descripción
--------|-------- 
RV_DOCUMENT_FLOW | Sacar el flujo de documentos en ventas | 
SD_OBJECT_TYPE_DETERMINE | Se le pasa el tipo de documento de SD y devuelve el objeto workflow asociado.


 # Ejemplos 

 ## RV_DOCUMENT_FLOW

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