---
title: Basic Component
description: Basic Component
bookToc: true
---

# Objetivo

Funciones del módulo basic component, es decir, funciones que son generales a todo SAP.

Al final hay una sección de ejemplos para algunas funciones. **No todas las funciones tienen ejemplo.**

# Lista

## Generales

Función | Descripción
--------|--------
SXO_IMPL_DELETE | Borra la implementación de una BADI
SCP_REPLACE_STRANGE_CHARS | Quita acentos, ç, ¨, etc… de una char
SELECTION_TEXTS_MODIFY_DTEL | Permite cambiar el texto de un campo de selección. Util para en enhacements en programas estándar.
SAPGUI_SET_FUNCTIONCODE | Permite simular que se pulsa un código de función.
SEO_METHOD_SIGNATURE_GET | De una clase y método te devuelve sus parametros y tipos de datos
C14Z_MESSAGES_SHOW_AS_POPUP | Saca un popup con los mensajes (formato similar a bapiret2) con sus iconos y demas
F4IF_INT_TABLE_VALUE_REQUEST | Permite hacer ayudas para búsqueda en base a una tabla interna
RS_REFRESH_FROM_SELECTOPTIONS |	Recupera los valores de los parametros de una pantalla de selección. Ideal para usarlo despues con el WITH SELECTION-TABLE
SYSTEM_CALLSTACK | Funciones para ver la pilas de llamada de un progama
BP_JOB_READ | Lee los datos de un job. El OPCODE para leerlo todo es el 36.
TH_IN_UPDATE_TASK | Función que devuelve si se esta corriendo en una cola de actualización. Si el parámetro de salida devuelve 1 se esta en un proceso actualización.


## Diccionario de datos

Función | Descripción
--------|--------
DOMAIN_VALUE_GET | Se le pone el dominio y valor, y devuelve su descripcion
DDUT_DOMVALUE_TEXT_GET | Recupera el texto de un valor fijo de un dominio
OIUP_REFRESH_TABLE_BUFFER | Sincroniza el buffer de una tabla

## SPOOL

Función | Descripción
--------|--------
RSPO_RETURN_SPOOLJOB | Devuelve el contenido binario de una orden de spool (para listados da dump). Si en el campo DESIRED_TYPE le pones "RAW" devuelve el contenido en formato legible. En este caso no da dump si es un ALF.
RSPO_DOWNLOAD_SPOOLJOB | Funcion que descarga un spool a fichero plano

## Conversión

Función | Descripción
--------|--------
UNIT_CONVERSION_SIMPLE | Convierte valores de unidades medidas estándar (de horas a minutos, etc..)
HRHAP_CONVERT_STR_TO_TABLE | Pasa de un string a un tline respetando saltos de lineas 

## Dirección

Función | Descripción
--------|--------
ADDR_GET | Devuelve los datos de direccion de cualqueir tipo de ADRNR
ADDR_GET_COMPLETE | Te devuelve todos los datos de direccion: direccion, mails, telefonos, etc..
ADDRESS_INTO_PRINTFORM | De una direccion te lo pinta en formato direccion de smartform. En el campo ADDRESS_TYPE hay que poner el valor '1'.

## Smartform

Función | Descripción
--------|--------
SSFRT_READ_TEXTMODULE | Devuele el texto creado por la transaccion SMARTFORMS

## ALV

Función | Descripción
--------|--------
LT_DBDATA_READ_FROM_LTDX | Permite leer los datos de una variante de ALV: catalogo, ordenaciones, etc..


 # Ejemplos 

 ## SELECTION_TEXTS_MODIFY_DTEL

 ```tpl
DATA: lt_zsel_dtel TYPE  rsseldtel OCCURS 0,
      ls_zsel_dtel TYPE  rsseldtel.

  ls_zsel_dtel-name = 'S_PRODPH'.
  ls_zsel_dtel-kind = 'S'.
  ls_zsel_dtel-datenelment = 'ZZE_PRODPH'.
  APPEND ls_zsel_dtel TO lt_zsel_dtel.

  CALL FUNCTION 'SELECTION_TEXTS_MODIFY_DTEL'
  EXPORTING
    PROGRAM                     = sy-repid
  TABLES
    sel_dtel                    = lt_zsel_dtel
  EXCEPTIONS
    program_not_found           = 1
    program_cannot_be_generated = 2
    OTHERS                      = 3.
```
## LT_DBDATA_READ_FROM_LTDX
```tpl
  CALL FUNCTION 'LT_DBDATA_READ_FROM_LTDX'
       EXPORTING
            I_TOOL       = R_TOOL " LT
            IS_VARKEY    = LS_VARKEY
       TABLES
            T_DBFIELDCAT = LT_DBFIELDCAT
            T_DBSORTINFO = LT_DBSORTINFO
            T_DBFILTER   = LT_DBFILTER
            T_DBLAYOUT   = LT_DBLAYOUT
       EXCEPTIONS
            NOT_FOUND    = 1
            WRONG_RELID  = 2
            OTHERS       = 3.


REPORT                                     ZMML0088
HANDLE                                    
LOG_GROUP                                    
USERNAME                                     JTORRE
VARIANT                                     TEST_ENZ
TYPE                                     F
```