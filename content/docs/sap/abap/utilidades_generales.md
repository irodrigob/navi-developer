---
title: Utilidades generales
description: Utilidades generales
bookCollapseSection: true
---

# Objetivo

Cajón de desastre de cosas que no se muy como clasificar.

# Hacks

## Saltarse seguridad como desarrollador

Para saltarse la seguridad del usuario desarrollador

 ![Saltarse seguridad](/images/sap/abap/util_general_saltarse_seguridad_desarrollador.png)

 # URL

 * http://wiki.sdn.sap.com/wiki/display/Basis/Timezone+changes+best+practices --> Best practices sobre los husos horarios

 # Rellenar campos con 0

 ```tpl
data: l_char(50).

TRANSLATE l_char USING ' 0'.
WRITE l_char. 
```
# Truco para saber que BADIS se utilizan en un proceso

Hay que ir a la transacción SE24 informar la clase *CL_EXITHANDLER* metodo e ir al contenido del método *GET_INSTANCE*. Hay que poner un break-point en la sentencia *CALL METHOD CL_EXITHANDLER=>GET_CLASS_NAME_BY_INTERFACE*. Con esto vas viendo cada una de las BADIs que se llaman mientras estas en la transacción, programa, etc.

# Poner sentencia SQL directamente en Produccion

Ir al programa *RSHOWTIM*. Ir al form *AUTHORITY_CHECK*. Despues que se lance la funcion TR_SYS_PARAMS poner en la variable SYS_CLIINDDEP_LOCK espacio. Si hay cualquier otro problema saltarselo con debugging.

# Saltarse seguridad SE16N

Debido a nota Note 1420281 - CO-OM tools: SE16N: Deactivating &SAP_EDIT. La opción &SAP_EDIT de la SE16N está deshabilitada por SAP a partir de un Support Package determinado. Para poder hacerlo basta con poner un breakpoint en el include LSE16NI01 linea 36: *CASE SAVE_OK_CODE*. Justo despues del *ENDCASE* (linea 203) actualizamos las variables gd-edit y gd-sapedit a 'X'.

# Ejecutar un programa automáticamente en una orden de transporte

Poner entrada: *R3TR XPRA RV80HGEN* para que se ejecute un programa, *RV80HGEN* despues de transporte. El programa en cuestión es el que regenera las rutinas de SD. Este truco es útil para la regeneracion de rutinas de SD.

# Usar comandos de SAPScript en cualquier sitio

```tpl
DATA ls_header TYPE thead.
DATA ls_line TYPE tline.
DATA lt_line TYPE TABLE OF tline.
ls_header-tdspras = sy-langu.
ls_header-tdlinesize = 72.
ls_line-tdformat = '/:'.
ls_line-tdline = 'SET DATE MASK = ''DD/MM/YY'''.
append ls_line to lt_line.
ls_line-tdformat = '*'.
ls_line-tdline = '&SY-DATUM&'.
append ls_line to lt_line.
CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
  EXPORTING
    HEADER = ls_header
  TABLES
    LINES = lt_line.
READ TABLE lt_line INTO ls_line INDEX 2.
WRITE : / ls_line-tdline.
```

# Ver un formulario en formato PDF en su previsualización

Cuando se esta previsualizando un spool se pone en la barra de comandos *PDF!* y se ve como si saliese en PDF.

# **Búsqueda de fechas**

Resalto en negrita el título porque esta chuleta me ha ido muy bien para hacer búsqueda en tablas con campos de fecha inicio y fin.

1. Registro válido para una fecha v_date: begda <= v_date and endda >= v_date
2. Registro comprendido íntegramente en un periodo f_ini - f_fin: begda <= f_ini and endda >= f_fin
3. Registro válido en algún momento del periodo f_ini - f_fin: begda <= f_fin and endda >= f_ini

# Usar simbolos sapscript para reemplazar valores

1. Con esto se establece el valor (se puede llamar tantas veces como se quiera):     
```tpl
CALL FUNCTION 'SET_TEXTSYMBOL'
      EXPORTING
        name  = '&GV_VIA_PAGO&'
        value = ld_via_pago.
```        
2. Con esta funcion se hace el reemplazo:

```tpl
CALL FUNCTION 'REPLACE_TEXTSYMBOL'
        EXPORTING
          endline   = 1
          startline = 1
        TABLES
          lines     = lt_text.
```          

# Capturar mensajes de error en funcion

Hay que añadir la excepcion ERROR_MESSAGE con un numero inferior a la excepción *OTHERS*.