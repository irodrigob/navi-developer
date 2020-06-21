---
title: Relleno de valores
description: Sentencias para informar tablas internas
---

# Introducción

En este grupo se irán poniendo las distintas sentencias, que son las que más uso, para poder informar tablas internas en base a otras tablas internas.

Estas sentencias se pueden utilizar para crear nuevas variables o ponerlas como entrada de parámetro en clases.

La sentencia estrella de esta página es la *FOR*. **Pero ojo: que si usamos field-symbols/variable no tiene que estar declarada previamente.**

# Ejemplo 1

Se recorren los datos de MT_TIPPQPS filtrando por dos campos y se rellenando los valores de ET_COND_CRUD con los datos de la estructura leída y de otras variables:

```tpl
et_cond_crud = VALUE #( FOR ls_tippqps IN mt_tippqps
                            WHERE ( pqtyp = is_header-negotiation_type_code
                                    AND keyfield = lv_keyfield )
                                    ( kschl = ls_tippqps-kschl
                                      kotab = ls_tippqps-kotab
                                      keyvalue = is_header-customer_code
                                      datab = is_header-validity_to
                                      datbi = is_header-validity_from
                                      updkz = zif_rtn_data=>cv_updkz_insert ) ).
```

Nota: Si *et_cond_crud* tuviera datos se perderían, si se quieren añadir los datos a los existentes hay que añadir la opción *BASE*:

```tpl
et_cond_crud = VALUE #( BASE  ( et_cond_crud ) FOR ls_tippqps IN mt_tippqps
                                             WHERE ( pqtyp = is_header-negotiation_type_code
                                                     AND keyfield = lv_keyfield )
                                            ( kschl = ls_tippqps-kschl
                                              kotab = ls_tippqps-kotab
                                              keyvalue = is_header-customer_code
                                              datab = is_header-validity_to
                                              datbi = is_header-validity_from
                                              updkz = zif_rtn_data=>cv_updkz_insert ) ).
```

# Ejemplo 2

Ampliación de la sentencia INSERT de siempre pero con en este caso no es necesario crear estructuras intermediante o usar el insert/append a un field-symbols y luego informar los valores

```tpl
INSERT VALUE #( sign = 'I' option = 'EQ' low = abap_false ) INTO TABLE lt_params_sl.
```