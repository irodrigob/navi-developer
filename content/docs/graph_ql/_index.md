---
title: GraphQL
description: GraphQL
weight: 20
bookCollapseSection: true
---

# Introducción

[GraphQL](https://graphql.org/) es un lenguaje que permite hacer consultas y operaciones (o mutaciones: añadir, actualizar, etc.) ¿Y con que podemos hacer eso? Pues con cualquier cosa: Desde ficheros JSON, base de datos como MongoDB o llamadas API Rest.

Este lenguaje es una alternativa a las típicas de API Rest ya que permite la obtención de datos desde varias fuentes de datos con una sola consulta. Esto con API Rest no es posible. 
Por lo que he estado leyendo GrapQL fui inventado por Facebook para reducir el numero de llamadas que se hacían para obtener información. Para lo que hacemos API Rest, en mi caso en SAP, entendemos el problema de muchas búsquedas para buscar datos de varios sitios.

En este [artículo](https://www.paradigmadigital.com/dev/graphql-todos-uno-uno-todos/) hay una explicación muy buena, y en castellano, de como es y como funciona GraphQL.


# Objetivo

El objetivo principal de los artículos es hacer una guía básica que explique como consultar y añadir/actualizar datos en una base de datos [MongoDB en el cloud](https://cloud.mongodb.com/) desde una aplicación NextJS. Lo hago NextJS porque es el framework que uso actualmente para hacer aplicación en React. Pero seguramente todo lo que explique puede usarse en otros framework de React.

En este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/) explico como he configurado mi base de datos en el cloud. Y usaremos la base de datos y collection creada como campo de pruebas.

# Aplicación

La aplicación de ejemplo la tengo públicada en [Github](https://github.com/irodrigob/nextjs-graphql-sample) por si alguien le quiere echar un vistazo.

En este página se explica como crear una aplicación NextJS pero por si caso pongo las sentencia necesaria:
```tpl
npx create-next-app nextjs-graphaql-sample
```

# Artículos

{{<section>}}