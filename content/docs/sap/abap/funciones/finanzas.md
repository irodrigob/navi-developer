---
title: Finanzas
description: Finanzas
bookToc: true
---

# Objetivo

Funciones del módulo de finanzas

# Lista

Función | Descripción
--------|-------- 
FI_FIND_PAYMENT_CONDITIONS | Obtiene la fecha de vencimiento en base a la fecha base y condicion de pago
GET_CURRENT_YEAR | Se le pasa sociedad y fecha de documento y devuelve el ejercicio fiscal
BAPI_COMPANYCODE_GET_PERIOD | Devuelve el periodo y ejercicio contable a partir de una fecha y sociedad. Esta funcion puede ser llamada en remota.
SD_PRINT_TERMS_OF_PAYMENT |	Calcula la fecha de vto
FI_TERMS_OF_PAYMENT_PROPOSE	| Calcula la fecha de vto. La diferencia con el anterior es que se le pasa el proveedor.
FI_F4_ZTERM | sTe muestra las condiciones de pago o devuelve una tabla con la información en base a un tipo de cuenta