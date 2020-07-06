---
title: Logística
description: Logística
bookToc: true
---

# Objetivo

Funciones del módulo de logística. Logística entraría la parte de compras, almacenes, etc.

# Lista

Función | Descripción
--------|--------
RM_KOKRS_TO_PLANT_FIND | De una centro te devuelve su sociedad FI y CO
CO_RM_COMPANYCODE_FIND | De una centro te devuelve su sociedad FI
RK_KOKRS_FIND | De una centro te devuelve su sociedad CO
CS_WHERE_USED_MAT |	De un material devuelve en que lista de material se usa.
ME_READ_HISTORY	| Historial de documento de compras
BAPISDORDER_GETDETAILEDLIST | Se le pasa los pedidos y que se desea recuperar y devuelve informacion completa del pedido
NAST_GET_MESSAGE_OBJECT_RECV | Función que devuelve el nombre de la lista de distribucion que esta en el campo NAST-TDNAME. *Nota: Se podría haber puesto también en ventas*
SO_DLI_READ_API1 | Devuelve los componentes de una lista de distribucion
CARO_ROUTING_READ |	De una hoja de ruta te devuelve sus componentes.
AC_DOCUMENT_RECORD | Te devuelve los documentos subsiguientes de un documento. I_AWREF es el numero de documento. I_AWORG es ejercicio.  I_AWTYP es el tipo de objeto. RMRP es para la MIRO.