---
title: Bucles
description: Sentencias sobre proceso de tablas internas
bookToc: true
---

# Introducción

En este grupo se irán poniendo las distintas sentencias que nos permiten procesar tablas internas. Este artículo esta muy relacionado con el del [relleno de valores](/docs/sap/abap/sentencias_74/relleno_valores.md) ya que se usán
las mismas sentencias para procesar tablas internas. Aún asi, lo separo en dos artículos según la finalidad en que se use.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

# Ejemplo 1

El siguiente ejemplo se usa la cláusula GROUP BY de los LOOP que sustituye a los AT NEW. 
En el primer loop nos devuelve la clave de registros únicos que se va agrupando y que informa en el field-symbol *<group>*. Y el *loop at group <group>* nos devuelve los registros que conforman dicha clave.

```tpl
LOOP AT mt_level_agr_type ASSIGNING FIELD-SYMBOL(<ls_level_agr_type>) WHERE id_nivel1 = 'OFF'
                         GROUP BY ( id_nivel1 = <ls_level_agr_type>-id_nivel1 id_nivel_inf = <ls_level_agr_type>-id_nivel_inf boart = <ls_level_agr_type>-boart
                                    size = GROUP SIZE index = GROUP INDEX  )
                         ASSIGNING FIELD-SYMBOL(<group>).
  .

write:/ <group>-id_nivel1, <group>-id_nivel_inf, <group>-boart.

  LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_level_agr_type_row>).
    WRITE:/ <ls_level_agr_type_row>-boart_agr.

  ENDLOOP.

ENDLOOP.
```

# Ejemplo 2

En este ejemplo es similar al anterior la diferencia es que dentro del loop ya tendremos los registros únicos según lo que se indique en el GROUP BY.

```tpl
        LOOP AT mt_key_field_text_cond ASSIGNING FIELD-SYMBOL(<ls_key_field_text_cond_group>)
                                       WHERE kschl = <ls_kschl>-kschl
                                             AND field_cust_is_hier = zcl_rtn_customer_data=>is_customer_hierarchy( iv_customer )
                                             AND field_cust IS NOT INITIAL
                                       GROUP BY <ls_key_field_text_cond_group>-kotab.
```
En la clausula GROUP BY se indica los campos que se quiere hacer la agrupación

# Ejemplo 3

Ejemplo encontrado de como hacer un collect:

```tpl
actual_collect = VALUE #( FOR GROUPS group OF <flight> IN original_table
                            GROUP BY ( carrid = <flight>-carrid
                                       connid = <flight>-connid )
                            ( REDUCE #( INIT sum_aux TYPE sflight
                                FOR line IN GROUP group
                                NEXT sum_aux = VALUE #( BASE sum_aux
                                   carrid     = line-carrid
                                   connid     = line-connid
                                   price      = sum_aux-price      + line-price
                                   seatsmax   = sum_aux-seatsmax   + line-seatsmax
                                   seatsocc   = sum_aux-seatsocc   + line-seatsocc
                                   paymentsum = sum_aux-paymentsum + line-paymentsum
                                   seatsmax_b = sum_aux-seatsmax_b + line-seatsmax_b
                                   seatsocc_b = sum_aux-seatsocc_b + line-seatsocc_b
                                   seatsmax_f = sum_aux-seatsmax_f + line-seatsmax_f
                                   seatsocc_f = sum_aux-seatsocc_f + line-seatsocc_f ) ) ) ).

```

# Ejemplo collect

En este ejemplo es propio y permite saber cuantas repeticiones que hay de las price list(PLTYP)

```tpl
 TYPES: BEGIN OF ts_pltyp_count,
             pltyp TYPE knvv-pltyp,
             count TYPE i,
           END OF ts_pltyp_count.
    TYPES: tt_pltyp_count TYPE STANDARD TABLE OF ts_pltyp_count WITH EMPTY KEY.

        " Se cuenta cuantas price list hay repetidas
        DATA(lt_count) = VALUE tt_pltyp_count( FOR GROUPS lt_group OF <wa1> IN lt_customer_sales
                                          GROUP BY ( pltyp = <wa1>-pltyp )
                                          ( REDUCE #( INIT ls_count TYPE ts_pltyp_count
                                                      FOR ls_group IN GROUP lt_group
                                                      NEXT ls_count = VALUE #( BASE ls_count
                                                                               pltyp = ls_group-pltyp
                                                                               count = ls_count-count + 1 ) ) ) ).
```

Este es un caso, cuya legibilidad no queda clara y tengas que leer la sentencias varias veces para entenderla. Sobretodo porque son cosas que, al menos a mi, no se usan muy a menudo.


# Ejemplo de un *DELETE ADJACENT DUPLICATES* con la sentencia *REDUCE*

```tpl
TYPES: BEGIN OF ts_dates_intervals,
         datab TYPE datab,
         datbi TYPE datab,
       END OF ts_dates_intervals.
TYPES: tt_dates_intervals TYPE STANDARD TABLE OF ts_dates_intervals WITH EMPTY KEY.

DATA(lt_dates_intervals) = VALUE tt_dates_intervals(
                    ( datab = '20200101' datbi = '20201231' )
                    ( datab = '20200101' datbi = '20201231' )
                    ( datab = '20200101' datbi = '20211231' )
                    ( datab = '2190301' datbi = '20211231' )
                    ( datab = '2190301' datbi = '20200830' )  ).

DATA(lt_dates) = VALUE tt_dates_intervals( FOR GROUPS lt_group OF <wa> IN lt_dates_intervals
                                           GROUP BY ( datab = <wa>-datab datbi = <wa>-datbi )
                                           ( REDUCE #( INIT ls_dates TYPE ts_dates_intervals FOR ls_group IN GROUP lt_group NEXT ls_dates = VALUE #( BASE ls_dates datab = ls_group-datab datbi = ls_group-datbi ) ) ) ).
```

Otra sentencia que usando las sentences de siempre quedan más claras para cualquiera que las lea. 

# Contar el valor de un campo en una tabla interna

Ejemplo que cuenta los registros que cumplen unas determinas condiciones:

```tpl
cs_layout_options-number_columns_fixed = REDUCE #( INIT x = 0 FOR <fcat_ui5> IN it_fieldcatalog_ui5 WHERE ( fixed_column = abap_true ) NEXT x = x + 1 ).
```

# Concatenar todos los valores de un campo de una tabla separandolos por un cáracter separador

Se puede hacer de tres formas:
1. Ejemplo 1 
```tpl
DATA(lv_sql) = REDUCE string( INIT sql TYPE string 
                              FOR <ls_fields> IN lt_fields 
                              NEXT sql = sql && COND #( LET sep = ',' IN WHEN sql IS NOT INITIAL 
                                                        THEN |{ sep } { <ls_fields> }| 
                                                        ELSE |{ <ls_fields> }| ) ).
```
2. Ejemplo 2
```tpl
DATA(lv_sql) = REDUCE string( INIT sql TYPE string 
                              FOR <ls_fields> IN lt_fields 
                              NEXT sql = sql && COND #( WHEN sql IS NOT INITIAL 
                                                        THEN |, { <ls_fields> }| 
                                                        ELSE |{ <ls_fields> }| ) ).
```
3. Ejemplo 3
```tpl
DATA(lv_sql) = REDUCE string( INIT sql TYPE string 
                              FOR <ls_fields> IN lt_fields 
                              NEXT sql = sql && CONV string( LET sep = COND #( WHEN sql IS NOT INITIAL 
                                                                               THEN ',' 
                                                                               ELSE '' ) IN |{ sep } { <ls_fields> }| ) ).
```

Todas ellas casi funcionan de la misma manera, la diferencia es como se concatena los campos. Casi, porque el ejemplo 3 pone un espacio en blanco antes del primer campo.

Como nota personal el código que finalmente use para el desarrollo que estaba haciendo es el número 2.