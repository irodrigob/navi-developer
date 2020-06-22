---
title: CDS
description: Core Data Services
weight: 20
---

# Objetivo

Los de CDS que aparecen, si no recuerdo mal, en la versión 7.4 de ABAP pero es en la versión 7.5, donde creo, tiene ya una funcionalidad madura que permite usarse en multitud de sitios.

Los CDS están pensados para y por HANA, aunque no es necesario tener HANA para usarlo pero según que funcionalidad de que uses pueden perder rendimiento, ya que disponen de funcionalidades que permite que el trabajo "sucio" lo haga la base de datos.
Además, los CDS permiten un Open SQL extendido, que se asemeja cada vez más al standard SQL, que permiten realizar operaciones sin necesidad de tratar a posterior los datos en ABAP.

Y punto importante como los CDS se ejecutan directamente en base de datos son más óptimos que hacer un SELECT tradicional a la hora de buscar y/o procesar un volumen alto de datos.

# Publicaciones

{{<section>}}