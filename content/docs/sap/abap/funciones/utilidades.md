---
title: Utilidades
description: Utilidades
bookToc: true
---

# Objetivo

Utilidades a la hora de usar o llamar a las funciones

# Capturar _RAISE_ en las funciones

Las excepciones de las funciones se capturan con la opción _EXCEPTIONS_, si usamos el botón de _Patrón_ ya se inserta. Hay una excepción que se añade siempre cuando se usa la opción de _Patrón_ que es la _OTHERS_ con valor 99. Esta excepción permite captura cualquier excepción que no hayamos declarado en _EXCEPTIONS_. Cuando se lanza dicha excepción la variable _SY-SUBRC_ valdrá el valor del indicado en _OTHERS_

La excepción _OTHERS_ se ha de ponerse siempre en las llamadas a funciones porque nos garantiza que si en un futuro se añade una excepción a la función estará será capturada aunque no la tengamos en _EXCEPTIONS_. El valor que se le debe de poner es 99, es el valor que le pone por defecto SAP, para evitar colisiones con otras execpiones.

# Captura mensajes de error

## Como excepción

Dentro de las funciones cuando hay un mensaje de error _message e001(fb)_ provoca que se salga de la función completamente. Para poder capturar dichos mensajes hay que añadir en _EXCEPTIONS_ la excepción _ERROR_MESSAGE_ con un valor inferior al _OTHERS_, otra excepción que no puede faltar.

## Mediante *TRY..*CATCH\*

Una nueva de captura es a través de los _TRY..CATCH_:

```tpl
CATCH cx_sy_message_in_plugin_mode INTO DATA(lx_message).

        INSERT zcl_ca_utilidades=>fill_return( i_type  = 'E'
                                               i_id         =lx_message->msgid
                                               i_number     =lx_message->msgno
                                               i_message_v1 =lx_message->msgv1
                                               i_message_v2 =lx_message->msgv2
                                               i_message_v3 =lx_message->msgv3
                                               i_message_v4 =lx_message->msgv4
                                               i_langu      = iv_langu ) INTO TABLE et_return. 
```

En la excepción tendremos los datos del mensaje generado
