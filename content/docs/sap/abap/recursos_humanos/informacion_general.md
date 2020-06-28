---
title: Información general
description: Información general
---

# Objetivo

Esta página tendrá un listado de información general sobre el módulo de recursos humanos

# Funciones

Función | Descripción
--------|--------
HR_READ_INFOTYPE_AUTHC_DISABLE | Hace que en la funcion HR_READ_INFOTYPE no haga control de autorizacion. Se ha de poner por cada función de lectura de infotipo
HR_CHECK_AUTHORITY_INFTY | Permite saber si se tiene autorizacion para leer/escribir un infotipo

## Ejemplos

### HR_CHECK_AUTHORITY_INFTY

```tpl
CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY' 
        EXPORTING 
          pernr            = pernr-pernr 
          infty            = '0008' 
          subty            = '0   ' 
          begda            = p0001-begda 
          endda            = p0001-endda 
          level            = 'R' 
          uname            = sy-uname 
        EXCEPTIONS 
          no_authorization = 1 
          internal_error   = 2 
          OTHERS           = 3. 
```


# Transacciones

Transacción | Descripción
--------|--------
PC_PAYRESULT | Programa para ver resultados de nomina
