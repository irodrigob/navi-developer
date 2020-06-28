---
title: Información general
description: Información general
---

# Objetivo

Esta página tendrá un listado de información general de Gateway

# Transacciones

Transacción | Descripción
--------|--------
SMGW | Transaccion para ver el monitor
SEGW | Transacción para los servicios de Gateway
/IWFND/CACHE_CLEANUP | Transaccion para borrar el cache del modelo
/IWFND/ERROR_LOG | Transaccion para ver los logs 
/IWFND/MAINT_SERVICE | Activar y mantener servicios

# Como debugear error XML Parse

Los errores de gateway muchas nos hay por donde cogerlos. Sobreto cuando se hace deep entity que no hay manera de saber que produce el error. He aquí una manera de saberlo a través del debugging:

Ir a la clase */IWCOR/CL_DS_EP_READER_JSON* método *READ_ENTITY_INTERNAL*. Este método procesa el JSON que recibe y se puede ver el fallo de mapeos



