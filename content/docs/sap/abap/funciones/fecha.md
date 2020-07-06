---
title: Fecha / Hora
description: Fecha y Hora
bookToc: true
---

# Objetivo

Debido a que hay multitud de funciones para fechas y horas que uso, las pongo en un apartado aparte en vez de ponerlas en la de *Basic Component*.

# Lista

Función | Descripción
--------|--------
GET_WEEK_INFO_BASED_ON_DATE | Te devuelve la semana y el día que es lunes y domingo.
DATE_GET_WEEK |	Solo devuelve la semana. Ojo que la semana viene al principio con el año.
HR_AUPBS_MONTH_DAY | Le pones dos fechas y te devuelve la diferencia en días, meses, años y días de calendario
HR_MONTHS_BETWEEN_TWO_DATES	| Dos fechas y te devuelve la diferencia en meses
MONTH_NAMES_GET	| Devuelve los textos de los meses para un idioma.
RP_CALC_DATE_IN_INTERVAL | Permite obtener una fecha a partir de meses, dias o años. Se puede indicar que se sumen o resten
SD_DATETIME_DIFFERENCE | Le pasas dos fechas y horas y te devuelve la diferencia en dias y horas
SWI_DURATION_DETERMINE	Funcion que devuelve en segundos la diferencia entre dos fechas y dos horas
WEEK_GET_FIRST_DAY | Devuelve el primer día de la semana pasada
TZ_SYSTEM_TO_LOCAL | Convierto la hora y fecha del huso horario del servidor a un huso horario
L_MC_TIME_DIFFERENCE | Es como la SD_DATETIME_DIFFERENCE pero devuelve la diferencia en minutos
LAST_WEEK |	De una semana te devuelve la fecha del lunes y domingo. Y además la semana anterior.
CONVERT_DATE_TO_INTERNAL | Convierte una fecha en char a formato interno. 
FIMA_DAYS_AND_MONTHS_AND_YEARS | Devuelve la diferencia en meses y años entre dos fechas
RSARCH_DATE_CONVERT	| De una fecha te devuelve mes, periodo, dia, semana, mes, etc..
EWU_ADD_TIME | Le pasas fecha y hora. Le pones un campo fecha para que sume hora/minuto/segundo
BKK_ADD_WORKINGDAY | Suma/resta dias a una fecha teniendo en cuenta dias laborables