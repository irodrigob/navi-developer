---
title: Condiciones
description: Sentencias sobre condiciones
---

# Introducción

En este grupo se irán poniendo las distintas sentencias que nos permiten hacer condiciones.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

# Ejemplo 1

Permite que una variable se inicialice segun una condicion determinada:

```tpl
DATA(lv_updkz) = COND #( WHEN is_header-negotiation_code IS INITIAL THEN zif_rtn_data=>cv_updkz_insert 
                         WHEN is_header-has_changed = abap_true THEN zif_rtn_data=>cv_updkz_edit ).
```

# Ejemplo 2

Uso de SWITCH haciendo una conversion de variable;

```tpl
 <ls_cond_crud>-value = SWITCH #( <ls_conditions_values_field>-field_type
                                   WHEN zif_rtn_negotiation_data=>cs_field_types-flex_group THEN <field>
                                   LSE CONV string( <field> ) ).
```                                             

# Ejemplo 3

```tpl
DATA lv_valor TYPE string.

DO 10 TIMES.
  lv_valor = COND #( LET x = sy-index MOD 2 IN WHEN x = 0 THEN |par| ELSE |impar|  ).
  WRITE:/ lv_valor.
ENDDO.
```