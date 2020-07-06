---
title: API desde ABAP
description: Como usar la API de los BOPF desde ABAP
bookToc: true
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

A la tabla donde se guarda los datos que se van a enviar siempre se guarda la estructura de datos. Esa estructura es siempre la combinada, la que tiene los datos persistentes y transitorios. El motivo es que esa estructura tiene el
campo donde le indicaremos la clave del registro.

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

Este ejemplo es cuando se añaden datos que dependen del root, además es un buen ejemplo para ver como se añaden datos de una tabla interna. Aquí de nuevo se usa la estructura combinada, que se instancia 
por cada nuevo registro. Si no lo hicieramos insertaríamos siempre el último registro debido a los punteros de memoria.

```tpl
    LOOP AT it_positions ASSIGNING FIELD-SYMBOL(<ls_positions>).
      DATA(lv_tabix) = sy-tabix.

      DATA(lo_positions) = NEW zcar_bo_sc_positions(  ).
      ASSIGN lo_positions->* TO FIELD-SYMBOL(<ls_bo_positions>).

      <ls_bo_positions> = CORRESPONDING #( <ls_positions> ).
      <ls_bo_positions>-posnr = lv_tabix.
      <ls_bo_positions>-key = /bobf/cl_frw_factory=>get_new_key( ).

      INSERT VALUE #( node = zif_car_bo_orders_c=>sc_node-positions
                change_mode = /bobf/if_frw_c=>sc_modify_create
                key = <ls_bo_positions>-key
                data = lo_positions
                source_node = zif_car_bo_orders_c=>sc_node-root
                association = zif_car_bo_orders_c=>sc_association-root-positions
                source_key = lo_header->key ) INTO TABLE lt_mod.

    ENDLOOP.
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

# Consulta de datos

Cuando se tiene la clave del registro del nodo se tiene que usar el método *retrieve* o *retrieve_by_association*. 

## Cuando se tiene el ID del registro

*LT_HEADER* es la tabla interna que se indica en *Combinated table type*

```tpl
 mo_svc_mngr->retrieve( EXPORTING iv_node_key             = zif_atron_file_engine_c=>sc_node-root
                              it_key                  = VALUE #( ( key = iv_id_file ) )
                              iv_fill_data            = abap_false
                    IMPORTING et_data                 = lt_header ).

```

## Los datos de un subnodo

En *node_key* se informa el nodo donde pertenece la clave que se le pasará *IT_KEY*. En asociación se indica del nodo de la key a que nodo se quiere recuperar valores. En el ejemplo la key pertenece al nodo *root* y queremos que recuperar los datos asociados a dicha clave en el nodo *content*.

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

## Hacer consultas

Al BOPF se puede hacer búsqueda por cualquier campo de los nodos que tenga. Para eso el nodo tiene que tener un tipo de búsqueda de tipo *Select by elements" en la pestaña *Query*.

Los parámetros de búsqueda se pasan en el tipo tabla */BOBF/T_FRW_QUERY_SELPARAM* y el método ha usar es el *QUERY*. Ejemplo:

```tpl
  DATA lt_header_selparams TYPE /bobf/t_frw_query_selparam.

  INSERT VALUE #( attribute_name = zif_car_bo_orders_c=>sc_node_attribute-root-vkorg sign = 'I' option = 'EQ' low = p_vkorg ) INTO TABLE lt_header_selparams.
  INSERT VALUE #( attribute_name = zif_car_bo_orders_c=>sc_node_attribute-root-vtweg sign = 'I' option = 'EQ' low = p_vtweg ) INTO TABLE lt_header_selparams.
  INSERT VALUE #( attribute_name = zif_car_bo_orders_c=>sc_node_attribute-root-spart sign = 'I' option = 'EQ' low = p_spart ) INTO TABLE lt_header_selparams.

  mo_svc_mngr->query(
  EXPORTING
    iv_query_key            = zif_car_bo_orders_c=>sc_query-root-select_by_elements
    it_selection_parameters = lt_header_selparams
    iv_fill_data            = abap_true
  IMPORTING
    et_data                 = et_header ).
```

El parámetro *IV_FILL_DATA* tiene que estar *TRUE* para que nos devuelva datos en el parámetro *ET_DATA*. La tabla interna donde se almacenan los datos debe ser del tipo *Combinated table type* que hay definida en el nodo.
Si solo queremos los ID de los registros se haría de la siguiente manera:

```tpl
 mo_svc_mngr->query( EXPORTING iv_query_key            = zif_car_bo_orders_c=>sc_query-positions-select_by_elements
                                      it_selection_parameters = it_positions_selparams
                                      iv_fill_data            = abap_false
                            IMPORTING et_key = DATA(lt_key_pos) ).
```

El parámetro de entrada *IS_QUERY_OPTIONS* se le puede indicar los campos de ordenación, número de registros y opciones de páginación.