---
title: Transacciones
description: Transacciones
---

# Objetivo

Recopilación de las transacciones que me parecen interesantes.

# Transacciones

{{< tabs "uniqueid" >}}
{{< tab "SAP R/3 o ECC" >}}

## Generales

Transacción | Descripción
--------|--------
I18N | Internacionalizacion. Permite configurar el smartforms para usar word (a partir del EHP4 lo hace este report: RSCPSETEDITOR)
CG3Y | Descarga de ficheros del servidor a local
CG3Z | Subido de fichero al servidor desde local
SM01 | Bloquea transacciones estándar
SLXT | Guarda las traducciones hechas en la transacción SE63
DWDM | Ejemplos controles de pantalla
BAPI | Permite ver los objetos de negocio con sus respectiva funcionalidad. Y sobretodo que función realiza dicha funcionalidad.

## Financieras

Transacción | Descripción
--------|--------
GGB0 | Transación global para validaciones
GGB1 | Transación global para sustituciones
OV51 | Documentos modificacion deudores

## Monitarización SAP PO

Transacción | Descripción
--------|--------
SXMB_MONI | Monitor de mensajes XML. Lo que viene de PI con ABAP Proxy

## Comunicaciones

Transacción | Descripción
--------|--------
SMQ2 | Colas qRFC
SCOT | Configuración de los canales de comunicación: mail, fax, etc.
SOST | Transacción que gestiona la salida de los canales de comunicación configurado: mail, fax, etc.
SOSG | Como la SOST pero solo de tu usuario

## IDOC

Transacción | Descripción
--------|--------
WEDI | Menu ambito de IDOCS

## Recursos humanos

Transacción | Descripción
--------|--------
PU00 | Borrar empleado de manera individual (programa RPUDELPN para masivos)
PC_PAYRESULT | Ve los resultados de nomina de un empleado

## Trace

Transacción | Descripción
--------|--------
ST12 | Hibrido entre la SAT y ST05 para temas de rendimiento ya sea web, programa, etc..

## Para SAP BW

Transacción | Descripción
--------|--------
RSO2 | Creacion extractores para BW


{{< /tab >}}

{{< tab "SAP BW" >}} 

## Generales

Transacción | Descripción
--------|--------
RSINPUT | Permite rellenar registros en un ODS de BW

{{< /tab >}}
