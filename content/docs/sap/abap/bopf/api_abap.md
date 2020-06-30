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

# Añadir registro 

Cuando se añaden hay que tener en cuenta si se insertados datos dependiente de un nodo superior, o no. En un BOPF todos los nodos dependen del ROOT, al menos es lo que he visto, por lo tanto
añadir en el nodo ROOT es distinto que en un nodo "hijo". Se muestra los ejemplos en ambos

## Nodo root

Cuando solo inserto un registro, por ejemplo un registro de cabecera, instancio en un objeto la estructura de los datos combinados porque contiene todos los campos necesarios para pasarle al BOPF.
En la combinada contiene también los campos transitorios que nunca se rellenan porque son calculados. Pero se tiene que usar la combinada para pasarla la clave a insertar.

Si se inserta datos de una tabla entonces se usa el tipo tabla definido en el BOPF.

```tpl
 DATA lt_mod TYPE /bobf/t_frw_modification.

    DATA(lo_header) = NEW zcar_bo_sc_header(  ).
    ASSIGN lo_header->* TO FIELD-SYMBOL(<ls_header>).
    <ls_header> = CORRESPONDING #( is_header ).
    <ls_header>-vbeln = iv_vbeln.
    <ls_header>-key = /bobf/cl_frw_factory=>get_new_key( ).

    INSERT VALUE #( node = zif_car_bo_orders_c=>sc_node-root
                          change_mode = /bobf/if_frw_c=>sc_modify_create
                          key = lo_header->key
                          data = lo_header )
                 INTO TABLE lt_mod.
```
# Subnodo del ROOT

Esto es cuando se añaden/modifican datos de un subnodo. Este ejemplo es la segunda parte del anterior. Ya qye en es

```tpl
    DATA(lo_positions) = NEW zcar_bo_i_positions(  ).
    ASSIGN lo_positions->* TO FIELD-SYMBOL(<lt_positions>).
    LOOP AT it_positions ASSIGNING FIELD-SYMBOL(<ls_positions>).
      DATA(lv_tabix) = sy-tabix.
      APPEND INITIAL LINE TO <lt_positions> ASSIGNING FIELD-SYMBOL(<ls_bo_positions>).
      <ls_bo_positions> = CORRESPONDING #( <ls_positions> ).
      <ls_bo_positions>-posnr = lv_tabix.
      <ls_bo_positions>-key = /bobf/cl_frw_factory=>get_new_key( ).
    ENDLOOP.

    INSERT VALUE #( node = zif_car_bo_orders_c=>sc_node-root
                change_mode = /bobf/if_frw_c=>sc_modify_create
                key = lo_header->key
                data = lo_positions
                source_node = zif_car_bo_orders_c=>sc_node-root
                association = zif_car_bo_orders_c=>sc_association-root-positions
                source_key = lo_header->key ) INTO TABLE lt_mod.
```

# Modificar registro

Este ejemplo es como se haría en el nodo ROOT pero se puede extrapolar como se haría con el ejemplo de añadir en un nodo dependiente del ROOT.

Cuando se modificar siempre hay que leer los datos previamente a través de la *querys* del BOPF. Una vez leído los datos se puede modificar.

```tpl

INSERT VALUE #( node = zif_sat_orders_c=>sc_node-root
                      change_mode = /bobf/if_frw_c=>sc_modify_update
                      key = lo_header->key
                      data = lo_header )
             INTO TABLE et_mod.
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