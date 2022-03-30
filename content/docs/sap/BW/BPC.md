---
title: BPC
description: Información general de BPC
---

# Objetivo

Como hago poco de BPC agruparé en esta página lo que haga de dicho módulo.

# Cambiar variables de secuencias de planificación en BPC

En este programa cambia los valores de variables para secuencias de planificación.

```tpl
*&---------------------------------------------------------------------*
*& Report ZRTC_CUSTOM_PROCESS
*&---------------------------------------------------------------------*
*& Programa de ejemplo para actualizar las variantes de secuencias de
*& planificacion que se definen en la transacción RSPLAN.
*&---------------------------------------------------------------------*
REPORT zrtc_custom_process.

TABLES: /bi0/pfiscper.

CONSTANTS cv_period_variable TYPE rsr_s_var_id VALUE 'ZVAR_RTC_PF_PERIOD'.

TYPES: BEGIN OF ts_result_data,
         seqnam       TYPE rspls_seqnm,
         variant      TYPE rsr_s_para_catalog-paramnm,
         variant_desc TYPE rsr_s_para_catalog-txtlg,
         msg_type     TYPE bapiret2-type,
         message      TYPE bapiret2-message,
       END OF ts_result_data.
TYPES: tt_result_data TYPE STANDARD TABLE OF ts_result_data WITH DEFAULT KEY.

DATA: mt_result_data TYPE tt_result_data.
DATA mv_variant TYPE rsplfrparamnm.
DATA mv_variant_desc TYPE rsr_s_para_catalog-txtlg.


************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  SELECT-OPTIONS s_period FOR  /bi0/pfiscper-fiscper OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS : p_prev AS CHECKBOX DEFAULT '' .
  PARAMETERS : p_nsap AS CHECKBOX DEFAULT '' .
  PARAMETERS : p_bcf AS CHECKBOX DEFAULT '' .
  PARAMETERS : p_appr AS CHECKBOX DEFAULT '' .
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  CLEAR: mt_result_data.

  IF p_prev EQ abap_true.

    DATA(lv_error_prev) = abap_false.

    PERFORM save_values_variant USING 'ZPS_RTC_STEP_1' CHANGING lv_error_prev mv_variant mv_variant_desc.
    IF lv_error_prev = abap_false.
      PERFORM execute_plseq USING 'ZPS_RTC_STEP_1' mv_variant mv_variant_desc.
    ENDIF.
  ENDIF.


  IF p_nsap EQ abap_true.

    DATA(lv_error_nsap) = abap_false.

    PERFORM save_values_variant USING 'ZPS_RTC_ACTUAL_NOSAP' CHANGING lv_error_nsap mv_variant mv_variant_desc.
    IF lv_error_nsap = abap_false.
      PERFORM execute_plseq USING 'ZPS_RTC_STEP_1' mv_variant mv_variant_desc.
    ENDIF.

  ENDIF.


  IF p_bcf EQ abap_true.

    DATA(lv_error_bcf) = abap_false.

*    PERFORM save_values_variant USING 'ZPS_RTC_ACTUAL_NOSAP' CHANGING lv_error_bcf mv_variant mv_variant_desc.
    IF lv_error_bcf = abap_false.
*      PERFORM execute_plseq USING 'ZPS_RTC_STEP_1' mv_variant mv_variant_desc.
    ENDIF.

  ENDIF.


  IF p_appr EQ abap_true.
    DATA(lv_error_appr) = abap_false.

*    PERFORM save_values_variant USING 'ZPS_RTC_ACTUAL_NOSAP' CHANGING lv_error_appr mv_variant mv_variant_desc.
    IF lv_error_appr = abap_false.
*      PERFORM execute_plseq USING 'ZPS_RTC_STEP_1' mv_variant mv_variant_desc.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  PERFORM show_alv.


FORM save_values_variant USING pe_seqnam TYPE rspls_seqnm
                         CHANGING ps_error TYPE sap_bool
                                  ps_variant TYPE rsplfrparamnm
                                  ps_variant_desc TYPE rsr_s_para_catalog-txtlg.

  DATA lv_handle TYPE rsr_handle .
  DATA lt_return2 TYPE bapiret2_t.
  DATA lt_values TYPE rsr_t_variable_values.
  DATA lt_variants TYPE rsr_t_para_catalog.

  CLEAR: ps_error, ps_variant, ps_variant_desc.

  " Función que nos devuelve el handler de la secuencia de acceso
  CALL FUNCTION 'RSPLSSE_PLSEQ_SCREEN_NECESSARY'
    EXPORTING
      i_seqnm      = pe_seqnam
      i_objvers    = 'A'
    IMPORTING
      e_var_handle = lv_handle
    TABLES
      e_tk_return  = lt_return2[].

  IF NOT line_exists( lt_return2[ type = 'E' ] ).

    CLEAR: lt_return2.

    " Función que inicializa las variantes a partir del handler. Este paso es crítico ya que se
    " inicializan tablas y clases internas que son necesarias para luego poder leer y grabar las variantes.
    CALL FUNCTION 'RSPLW_SEQ_VAR_INIT'
      EXPORTING
        i_handle = lv_handle
      IMPORTING
        e_t_msg  = lt_return2.
    IF NOT line_exists( lt_return2[ type = 'E' ] ).

      PERFORM get_variants USING lv_handle CHANGING lt_variants.
      LOOP AT lt_variants ASSIGNING FIELD-SYMBOL(<ls_variants>).
        CLEAR lt_values.

        " Función que lee los valores de la variante
        CALL FUNCTION 'RSR_VAR_VARIANT_VALUE_SET'
          EXPORTING
            i_var_cont_handle = lv_handle
            i_variant         = <ls_variants>-paramnm
          IMPORTING
            e_t_values        = lt_values.


        " Localizamos la variable de periodo
        LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_values>) WHERE var_id CS cv_period_variable.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
*            INSERT VALUE #( var_id  = VALUE rsr_s_var_id( handle = lv_handle vnam = pe_seqnam ) )
*                   INTO TABLE lt_values ASSIGNING <ls_values>.
        ELSE.
          ps_variant = <ls_variants>-paramnm.
          ps_variant_desc = <ls_variants>-txtlg.

          PERFORM fill_values USING <ls_values> .
          PERFORM save_values USING lv_handle <ls_variants> pe_seqnam  CHANGING lt_values.

          EXIT. " Se sale porque ya la he encontrado

        ENDIF.

      ENDLOOP.
      " Si no hay variante es que no se ha encontrado por lo tanto se devuelve un error
      IF ps_variant IS INITIAL.
        INSERT VALUE #( seqnam = pe_seqnam
               msg_type = 'E'
               message = TEXT-e01 ) INTO TABLE mt_result_data.
      ENDIF.

    ELSE.
      PERFORM add_bapiret2_2_return_data USING pe_seqnam space space lt_return2.
    ENDIF.

  ELSE.
    PERFORM add_bapiret2_2_return_data USING pe_seqnam space space lt_return2.
  ENDIF.


  " Devuelvo si hay un error en el proceso
  ps_error = COND #( WHEN line_exists( mt_result_data[ seqnam = pe_seqnam msg_type = 'E' ] ) THEN abap_true ELSE abap_false ).


ENDFORM.

FORM add_bapiret2_2_return_data USING pe_seqnam pe_variant pe_variant_desc pe_return2 TYPE bapiret2_t.

  LOOP AT pe_return2 ASSIGNING FIELD-SYMBOL(<ls_return2>).

    INSERT VALUE #( seqnam = pe_seqnam
                    variant = pe_variant
                    variant_desc = pe_variant_desc
                    msg_type = <ls_return2>-type ) INTO TABLE mt_result_data ASSIGNING FIELD-SYMBOL(<ls_data>).

    MESSAGE ID <ls_return2>-id TYPE <ls_return2>-type NUMBER <ls_return2>-number
                INTO <ls_data>-message
                WITH <ls_return2>-message_v1 <ls_return2>-message_v2 <ls_return2>-message_v3 <ls_return2>-message_v4.

  ENDLOOP.

ENDFORM.

FORM execute_plseq USING pe_seqnam TYPE rspls_seqnm
                         pe_variant TYPE rsplfrparamnm
                         pe_variant_desc TYPE rsr_s_para_catalog-txtlg.

  DATA lt_return TYPE bapiret2_t.

  CALL FUNCTION 'RSPLSSE_PLSEQ_EXECUTE'
    EXPORTING
      i_seqnm     = pe_seqnam
      i_variant   = pe_variant
*     I_FAST_ENQUEUE       = RS_C_FALSE
    TABLES
      e_tk_return = lt_return[].

  PERFORM add_bapiret2_2_return_data USING 'ZPS_RTC_STEP_1' pe_variant pe_variant_desc lt_return.
ENDFORM.

FORM show_alv.
  DATA lo_alv TYPE REF TO cl_salv_table.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = mt_result_data.

      DATA(lo_columns) = lo_alv->get_columns(  ).
      lo_columns->set_optimize( abap_true ).

      DATA(lo_column) = lo_columns->get_column( 'VARIANT' ).
      lo_column->set_fixed_header_text( CONV #( TEXT-c01 ) ).
      lo_column->set_medium_text( CONV #( TEXT-c01 ) ).
      lo_column->set_long_text( CONV #( TEXT-c01 ) ).
      lo_column = lo_columns->get_column( 'VARIANT_DESC' ).
      lo_column->set_fixed_header_text( CONV #( TEXT-c02 ) ).
      lo_column->set_medium_text( CONV #( TEXT-c02 ) ).
      lo_column->set_long_text( CONV #( TEXT-c02 ) ).

      lo_alv->display(  ).

    CATCH cx_salv_msg.
  ENDTRY.
ENDFORM.

FORM get_variants USING pe_handle TYPE rsr_handle
                  CHANGING ps_variants TYPE rsr_t_para_catalog.

  CLEAR: ps_variants.

  CALL FUNCTION 'RSR_VAR_VARIANT_LIST_GET'
    EXPORTING
      i_var_cont_handle = pe_handle
    IMPORTING
      e_t_variant       = ps_variants.
ENDFORM.

FORM save_values USING pe_handle TYPE rsr_handle
                       pe_variant TYPE rsr_s_para_catalog
                       pe_seqnam TYPE rspls_seqnm
                 CHANGING ps_values TYPE rsr_t_variable_values.

  DATA lt_return TYPE bapiret2_t.


  " Pasamos los valores actualizados
  CALL FUNCTION 'RSR_VAR_SET_VALUES'
    EXPORTING
      i_var_cont_handle = pe_handle
      i_t_values        = ps_values
    IMPORTING
      e_t_return        = lt_return.

  IF NOT line_exists( lt_return[ type = 'E' ] ).
    CLEAR: lt_return.

    " Finalmente se graban los valores
    DATA(lv_variant_string) = |{ pe_variant-paramnm }|.
    CALL FUNCTION 'RSR_VAR_VARIANT_CHANGE'
      EXPORTING
        i_var_cont_handle = pe_handle
        i_variant_id      = lv_variant_string
      IMPORTING
        e_t_return        = lt_return.

    IF NOT line_exists( lt_return[ type = 'E' ] ).
      INSERT VALUE #( seqnam = pe_seqnam
             variant = pe_variant-paramnm
             variant_desc = pe_variant-txtlg
             msg_type = 'S'
             message = TEXT-s01 ) INTO TABLE mt_result_data.
    ELSE.
      PERFORM add_bapiret2_2_return_data USING pe_seqnam pe_variant-paramnm pe_variant-txtlg lt_return.
    ENDIF.
  ENDIF.
ENDFORM.

FORM fill_values CHANGING ps_value TYPE rsr_s_variable_values .
  CLEAR ps_value-values.

  " Se pasan los periodos a los valores.
  LOOP AT s_period ASSIGNING FIELD-SYMBOL(<s_period>).
    " Todos los campos que se informan son necesarios.
    INSERT VALUE #( sign = <s_period>-sign
                   opt = <s_period>-option
                   low = <s_period>-low
                   high = <s_period>-high
                   lowtxt_len = 20
                   lowtxt_tp = 'S' ) INTO TABLE ps_value-values ASSIGNING FIELD-SYMBOL(<ls_value_ranges>).

    " Este campo tiene el formato externo del periodo, sino se rellena, se graba un # como valor. Da igual si viene en el
    " campo low informado.
    <ls_value_ranges>-low_ext = |{ <ls_value_ranges>-low+4(3) }.{ <ls_value_ranges>-low(4) }|.

    " En el caso de tener un rango lo informo. El IS INITIAL no me chuta con lo cual he ido a la antigua usanza.
    IF <ls_value_ranges>-high NE '0000000'.
      <ls_value_ranges>-high_ext = |{ <ls_value_ranges>-high+4(3) }.{ <ls_value_ranges>-high(4) }|.
      <ls_value_ranges>-hightxt_len = 20.
      <ls_value_ranges>-hightxt_tp = 'S'.
    ENDIF.
  ENDLOOP.
ENDFORM.
```