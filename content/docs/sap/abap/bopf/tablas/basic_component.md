---
title: Basic Component
description: Basic Component
---

# Objetivo

Generalmente las tablas que generales a todo SAP son las interesantes ya que permiten cosas que de otra manera no se podría saber: ya sea porque no se conoce la transacción o falta de permisos para acceder a determinadas transacciones.

# Lista

## Generales

Función | Descripción
--------|--------
SXC_CLASS |	Contiene que clase esta utilizando una implementacion de BADI
DWINACTIV |	Objetos desactivados
SSM_CUST | Variables del session manager (ejemplo imagen en pantalla de inicio)
SSM_USR	| Tabla para controlar a que menus tiene acceso el usuario en el session manager
USERS_SSM |	Lo mismo que la de antes pero valida en la ECC6.0 EHP4
SE16N_CD_KEY | Change Documents – Header
SE16N_CD_DATA |	Change Documents – Data
DBTABLOG | Log de modificacion de tablas
TLOCK | Tabla de bloqueos de objetos en ordenes. 
BALHDR | Tabla de cabecera con logs que se generan con la SLG0 y SLG1

## Desarrollo ABAP

Función | Descripción
--------|--------
DEVACCESS |	Tabla con la clave desarrollador de los usuarios
TFDIR |	Include y programa de una funcion.

## Formularios

Función | Descripción
--------|--------
STXFTXT | Tabla de textos de formularios

## Archivelink / GOS / BDS / Etc.

Función | Descripción
--------|--------
SRGBTBREL |	Tabla con los adjuntos del GOS. Es como la TOA01 del archivelink
SGOSATTR | Tabla que sirve para configurar acciones que salen en el boton del GOS de SAP
SDOKFEXT_C | Tabla de cliente para poner los mimetype que faltan para que pueda funcionar el GOS