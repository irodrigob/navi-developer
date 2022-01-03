---
title: Schemas
description: Schemas GraphQL
---

# Introducción

Los schemas es la definición principal de los vas a poder hacer en GraphQL, es decir, se definen:

* Los tipos de datos que puedes usar
* Las querys que puedes llamar y como llamarlas.
* Las operaciones de edición, crear o modificar, que puedes realizar.

Si hago un simil con SAP Gateway es la definción de las Entitys cuando se crea un servicio oData. Todo los ejemplos que explicaré se basan en NodeJS usando el framework de React llamado Nextjs.

Ejemplo de schema:

```tpl
import { gql } from "apollo-server-micro";

export const typeDefs = gql`
  type Medidas {
    altura: Int
    anchura: Int
    unidad: String
  }
  type Product {
    _id: ID
    mueble: String!
    material: String
    medidas: Medidas
  }
  type Query {
    getAll: [Product]
    getMuebles(mueble: String!): [Product]
    getSingleMueble(mueble: String!): Product
  }

  type Mutation {
    newProduct(mueble: String!, material: String): Product
  }
`;
```

Todo lo que se define en GraphQL tiene que ir dentro de *gql``*. Y la estructura es como la de un JSON.

Para conocer más en detalle como funciona lo mejor es ir [documentación oficial](https://graphql.org/learn/schema/).

# Tipos de datos

Los tipos de datos es lo primero que se define porque luego se usarán para definir que datos devuelve las consultas o las mutaciones. Además, es posible indicar si un campo va ser obligatorio (se indica usando el singo *!* después del tipo de datos).

# Básicos

Tomamos como base este schema:

```tpl
const typeDefs = gql`
  type Medidas {
    altura: Int
    anchura: Int
    unidad: String
  }
  type Product {
    _id: ID
    mueble: String!
    material: String
    medidas: Medidas
  }
`;
```

Tenemos dos tipos definidos: Medidas y Product. El tipo principal sería *Product* que tiene los campos:

* _id -> Es el campo que añade mongoDB a los documentos que se insertan en una collection. El tipo de datos es ID porque es que indica a GraphQL que es un campo de ID único. Más info en la docu oficial.
* mueble -> De tipo de string pero le he puesto el signo *!* al final del tipo de campo para indicarle que este campo no puede ser *null*, es decir, que habrá que ponerlo en cualquier operación donde se use el tipo definido.
* material -> Campo string
* medidas -> Que es un campo que se basa en otro tipo de datos.

El tipo *Medidas* tendrá tres campos y que se usará como definición de un campo del tipo *Product*. Este campo servirá para ver como podemos datos dentro de un campo sin usar tablas relacionados como haríamos en base de datos SQL.



