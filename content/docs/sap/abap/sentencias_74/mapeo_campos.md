---
title: Mapeo de campos
description: Sentencias sobre mapeo de campos
---

# Introducción

En este grupo se irán poniendo las distintas sentencias que nos permiten hacer mapeo de campos, digamos que son las que sustituyen al *MOVE-CORRESPODING*.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

# Ejemplo 1

La clase CL_ABAP_CORRESPONDING permite crear mapeos entre tablas y estructuras. En este ejemplo se construye la tabla de mapeos en base a una tabla interna.
**Nota: Los campos que se informan en el mapeo deben de existir ya que sino se provoca una excepción.**

```tpl
 cl_abap_corresponding=>create(
   source            = is_header
   destination       = es_header_crud
   mapping           = VALUE cl_abap_corresponding=>mapping_table( FOR ls_mapping IN mt_mapping_crud
                                                            WHERE ( area = 'HEADER_NEGOTIATION' AND id_nivel1 = cv_level1 )
                                                            ( level = 0
                                                              kind = cl_abap_corresponding=>mapping_component
                                                              srcname = ls_mapping-field_from
                                                              dstname = ls_mapping-field_to ) ) )->execute( EXPORTING source      = is_header
                                                                                                            CHANGING  destination = es_header_crud ).
```

# Ejemplo 2

```tpl
<ls_summary_all> = CORRESPONDING #( BASE ( <ls_summary_all> ) <ls_datos> ).
```

Se mueven los campos de *<ls_datos>* a *<ls_summary_all>*. Esto por defecto hace que los campos que están en *<ls_summary_all>* pero no en *<ls_datos>* se dejan en blanco aunque tengan valores. Para eso hay que usar la cláusula BASE y entre paréntesis la estructura destino para que no limpie los campos que no tiene.
