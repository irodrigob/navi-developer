---
title: API desde ABAP
description: Como usar la API de los BOPF desde ABAP
---

# Objetivo

La API de acceso al ABAP es siempre la misma por eso voy a poner los ejemplos de cada tipo de operación que se quiera hacer.

Yo para estos casos me suelo crear un helper para simplificar las llamadas entre las clases del proceso y del BOPF.

# Instanciar variables para acceder al BOPF

Lo primero que se suele hacer es instanciar las variables necesarias para operar con los BOPF.

```tpl
DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
 TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( iv_bo_key ).

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.
```

# Añadir registro nuevo

```tpl
 ET_MOD type /BOBF/T_FRW_MODIFICATION
	
" Se instancia el objeto donde se guardarán los datos de cabecera.    
DATA(lo_header) = NEW zsat_bo_sc_orders_header( ).
“ El puntero es para poder hacer corresponding
    ASSIGN lo_header->* TO FIELD-SYMBOL(<ls_header>).

 lv_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_header> = CORRESPONDING #( is_header ).
      <ls_header>-key = /bobf/cl_frw_factory=>get_new_key( ).

INSERT VALUE #( node = zif_sat_orders_c=>sc_node-root
                      change_mode = lv_mode
                      key = lo_header->key
                      data = lo_header )
             INTO TABLE et_mod.
```

# Modificar registro

```tpl
 DATA(lv_mode) = /bobf/if_frw_c=>sc_modify_update.

“ Hay que buscar el registro actualizar con las opciones QUERY del BOPF y actualizar los campos

INSERT VALUE #( node = zif_sat_orders_c=>sc_node-root
                      change_mode = lv_mode
                      key = lo_header->key
                      data = lo_header )
             INTO TABLE et_mod.
```

# Informar estructura de modificación para subnodo

Esto es cuando se añaden/modifican datos de un subnodo.

```tpl
 INSERT VALUE #( node = zif_atron_file_engine_c=>sc_node-root
                change_mode = /bobf/if_frw_c=>sc_modify_create
                key = lo_header->key
                data = lo_content
                source_node = zif_atron_file_engine_c=>sc_node-root
                association = zif_atron_file_engine_c=>sc_association-root-content
                source_key = lo_header->key ) INTO TABLE lt_mod.
```
# Modificar datos en BOPF

Para que los datos del BOPF se graben primero se lanza el proceso de modificación, en este proceso es donde saltarían las validaciones propias.

En la clase de utilidades del BOPF que se esta indicada en el índice de la sección todo este proceso esta encapsulado para simplificar la operativa.

```tpl
   mo_svc_mngr->modify(
      EXPORTING
        it_modification = it_mod
      IMPORTING
        eo_change       = DATA(lo_change)
        eo_message      = eo_message ).

```
# Grabar datos en BOPF

Una vez la modificación no devuelve errores es cuando se pueden grabar.

En la clase de utilidades del BOPF que se esta indicada en el índice de la sección todo este proceso esta encapsulado para simplificar la operativa.

```tpl
 mo_txn_mngr->save(
        IMPORTING
          ev_rejected            = ev_rejected
          eo_message             = eo_message ).
```

# Leer datos por el ID de nodo

*LT_HEADER* es la tabla interna que se indica en *Combinated table type*

```tpl
 mo_svc_mngr->retrieve( EXPORTING iv_node_key             = zif_atron_file_engine_c=>sc_node-root
                              it_key                  = VALUE #( ( key = iv_id_file ) )
                              iv_fill_data            = abap_false
                    IMPORTING et_data                 = lt_header ).

```

# Leer por asociación

En *node_key* se informa el nodo donde pertenece la clave que se le pasará a *IT_KEY*. En asociación se indica del nodo de la key a que nodo se quiere recuperar valores. En el ejemplo la key pertenece al nodo *root* y queremos que recuperar los datos asociados a dicha clave en el nodo *content*.

```tpl
 mo_svc_mngr->retrieve_by_association(
        EXPORTING
          iv_node_key             = zif_atron_file_engine_c=>sc_node-root
          it_key                  =  VALUE #( ( key = iv_id_file ) )
          iv_association          = zif_atron_file_engine_c=>sc_association-root-content
          iv_fill_data = abap_true
        IMPORTING
           et_data                 = lt_content ).
```