---
title: Creación pedido de ventas
description: Ejemplo básico de creación pedido de ventas
---

# Ejemplo

Este es un ejemplo muy básico de creación de un pedidos ventas.

```tpl
" Tipos de datos
TYPES: tt_bapi_item TYPE STANDARD TABLE OF bapisditm WITH EMPTY KEY.
TYPES: tt_bapi_itemx TYPE STANDARD TABLE OF bapisditmx WITH EMPTY KEY.
TYPES: tt_bapi_partners TYPE STANDARD TABLE OF bapiparnr WITH EMPTY KEY.
TYPES: tt_bapi_schedules TYPE STANDARD TABLE OF bapischdl WITH EMPTY KEY.
TYPES: tt_bapi_schedulesx TYPE STANDARD TABLE OF bapischdlx WITH EMPTY KEY.

DATA lv_ebeln TYPE vbak-vbeln.

" Estructuras de la BAPI
DATA ms_bapi_header TYPE bapisdhd1.
DATA ms_bapi_headerx TYPE bapisdhd1x.
DATA mt_bapi_item TYPE tt_bapi_item.
DATA mt_bapi_itemx TYPE tt_bapi_itemx.
DATA mt_bapi_partners TYPE tt_bapi_partners.
DATA mt_bapi_schedules TYPE tt_bapi_schedules.
DATA mt_bapi_schedulesx TYPE tt_bapi_schedulesx.
DATA mt_bapi_return TYPE bapiret2_t.


" Datos de cabecera del pedido
ms_bapi_header-doc_type = 'ZP06'. " CLase pedido
ms_bapi_headerx-doc_type = abap_true.
ms_bapi_header-po_method = 'CSTK'. " Motivo de pedido
ms_bapi_headerx-po_method = abap_true.
ms_bapi_header-sales_org  = 'US01'. " Org. ventas
ms_bapi_headerx-sales_org  = abap_true.
ms_bapi_header-distr_chan = '10'. " Canal
ms_bapi_headerx-distr_chan = abap_true.
ms_bapi_header-division   = 'PP'. " Sector
ms_bapi_headerx-division   = abap_true.
ms_bapi_header-req_date_h = sy-datum. " Fecha preferente entrega
ms_bapi_headerx-req_date_h = abap_true.
ms_bapi_header-war_date = sy-datum. " Fecha de garantia. Es opcional
ms_bapi_headerx-war_date = abap_true.
ms_bapi_header-purch_no_c = 'Pedido cliente'.
ms_bapi_headerx-purch_no_c = abap_true.

ms_bapi_headerx-updateflag = 'I'. " Se va insertar el pedido

" Posiciones del pedido
DATA(lv_cont) = 1.

APPEND INITIAL LINE TO mt_bapi_item ASSIGNING FIELD-SYMBOL(<ls_bapi_items>).
APPEND INITIAL LINE TO mt_bapi_itemx ASSIGNING FIELD-SYMBOL(<ls_bapi_itemsx>).

<ls_bapi_items>-itm_number = lv_cont. " Posición del pedido
<ls_bapi_itemsx>-itm_number = lv_cont.
<ls_bapi_itemsx>-updateflag = 'I'. " Se va insertar la posición
<ls_bapi_items>-material = '000000000065032815'.
<ls_bapi_itemsx>-material = abap_true.
<ls_bapi_items>-target_qty = '12.000'.
<ls_bapi_itemsx>-target_qty = abap_true.
<ls_bapi_items>-target_qu = 'ST'.
<ls_bapi_itemsx>-target_qu = abap_true.

" La cantidad que se informa en la posición hay que indicarla también en el reparto
APPEND INITIAL LINE TO mt_bapi_schedules ASSIGNING FIELD-SYMBOL(<ls_bapi_schedules>).
APPEND INITIAL LINE TO mt_bapi_schedulesx ASSIGNING FIELD-SYMBOL(<ls_bapi_schedulesx>).

<ls_bapi_schedules>-itm_number = <ls_bapi_items>-itm_number.
<ls_bapi_schedulesx>-itm_number = <ls_bapi_items>-itm_number.
<ls_bapi_schedules>-req_qty = <ls_bapi_items>-target_qty.
<ls_bapi_schedulesx>-req_qty = abap_true.

" Partners
" Destinatario
APPEND INITIAL LINE TO mt_bapi_partners ASSIGNING FIELD-SYMBOL(<ls_partner>).
<ls_partner>-partn_role = 'WE'.
<ls_partner>-partn_numb = '0003331395'.

" Solicitante
APPEND INITIAL LINE TO mt_bapi_partners ASSIGNING <ls_partner>.
<ls_partner>-partn_role = 'AG'.
<ls_partner>-partn_numb = '0003331395'.

CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
    order_header_in     = ms_bapi_header
    order_header_inx    = ms_bapi_headerx
  IMPORTING
    salesdocument       = lv_ebeln
  TABLES
    return              = mt_bapi_return[]
    order_items_in      = mt_bapi_item
    order_items_inx     = mt_bapi_itemx
    order_partners      = mt_bapi_partners
    order_schedules_in  = mt_bapi_schedules
    order_schedules_inx = mt_bapi_schedulesx.

" Nota la clase de mensajes 'V4' es propia de la BAPI, se recomienda excluirlos para saber
" no tener mensajes que no aportan nada
READ TABLE mt_bapi_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type = 'E'.
IF sy-subrc NE 0.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  WRITE:/ 'Pedido creado: ', lv_ebeln.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  WRITE:/ 'Se ha producido el error:', <ls_return>-message.
ENDIF.
```