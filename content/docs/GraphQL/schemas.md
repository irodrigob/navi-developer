---
title: Schemas
description: Schemas GraphQL
---

# Introducción

Los schemas es la definición principal de los vas a poder hacer en GraphQL, es decir, se definen:

* Los tipos de datos que puedes usar
* Las querys que puedes llamar y como llamarlas.
* Las operaciones de edición, crear o modificar, que puedes crear.

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
  type Mitabla {
    _id: ID
    mueble: String!
    material: String
    medidas: Medidas
  }

  type Query {
    getAll: [Mitabla]
    getMuebles(mueble: String!): [Mitabla]
    getSingleMueble(mueble: String!): Mitabla
  }
`;
```

Todo lo que se define en GraphQL tiene que ir dentro de *gql``*. Y la estructura es como la de un JSON.

Para conocer más en detalle como funciona lo mejor es ir [documentación oficial](https://graphql.org/learn/schema/).

# Tipos de datos

Los tipos de datos es lo primero que se define porque se usarán para luego indicar que tipos de datos va a devolver una consulta o para añadir datos, o incluso si un campo es obligatorio.

# Básicos

Tomamos como base este schema:

```tpl
const typeDefs = gql`
  type Medidas {
    altura: Int
    anchura: Int
    unidad: String
  }
  type Mitabla {
    _id: ID
    mueble: String!
    material: String
    medidas: Medidas
  }
`;
```

Tenemos dos tipos definidos: Medidas y Mitabla. El tipo principal sería *Mitabla* que tiene los campos:

* _id -> Es el campo que añade mongoDB a los documentos que se insertan en una collection. El tipo de datos es ID porque es que indica a GraphQL que es un campo de ID único. Más info en la docu oficial.
* mueble -> De tipo de string pero le he puesto el signo *!* al final del tipo de campo para indicarle que este campo no puede ser *null*, es decir, que habrá que ponerlo en cualquier operación donde se use el tipo definido.
* material -> Campo string
* medidas -> Que es un campo que se basa en otro tipo de datos.

El tipo *Medidas* tendrá tres campos y que se usará como definición de un campo del tipo *Mitabla*. El motivo de hacerlo así es porque este schema se va en como inserto los datos en un ejemplo que tengo en MongoDB:
```tpl
db.mitabla.insertOne({
   {
      "mueble":"mesa",
      "material":"madrea",
      "medidas":{
         "altura":50,
         "anchura":30,
         "unidad":"cm"
      })
```



