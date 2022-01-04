---
title: Configurando servidor GraphQL
description: Configuando el servidor GraphQL
---

# Introducción

Este artículo esta basado es este [artículo](https://www.smashingmagazine.com/2020/10/graphql-server-next-javascript-api-routes/).Puede ser que los pasos que haga difieran un poco del artículo pero es lo que tiene basarse en un artículo de hace un año, y más como avanza de rápido estos lenguajes.

En este artículo haremos la instalación básica del servidor de GraphQL.

# Instalación

Antes de hacer la instalación he creado la siguiente estructura de carpetas en mi proyecto:

```tpl
|- pages
|--- api
|----- graphql.js
|----- resolvers
|------- index.js
|----- schemas
|------- index.js
```

Que es cada cosa:

* Carpeta API --> Es la carpeta que se usa para poder montar servicios en NextJS. Esta página cuando la aplicación esta corriendo se mapea a */api/**. Diriamos que es el endpoint de los servicios.
* Archivo graphql.js --> Es donde se indica la configuración para que arranque el servidor.
* Carpeta resolvers --> Es donde donde se ejecutan las consultas. En nuestro caso serán las llamadas a MongoDB. 
* Carpeta schemas --> Es donde se definen los tipos de datos que se usarán en las consultas o mutaciones. A modo de simil sería el WSDL en los Webservices o los Entitys en servicios REST.

El paquete que instalaremos es el *apollo-server-micro* mediante la sentencia:

```tpl
npm install apollo-server-micro
```

Este paquete esta pensando para Nextjs ya que el propio servidor se inicia cuando arrancamos la aplicación NextJS, evitando tener que arrancas dos servidor a la vez. 

A continuación en el fichero *graphql.js* escribiremos lo siguiente:

```tpl
import { ApolloServer } from "apollo-server-micro";
import { typeDefs } from "./schemas";
import { resolvers } from "./resolvers";

const apolloServer = new ApolloServer({ typeDefs, resolvers });

const startServer = apolloServer.start();

export default async function handler(req, res) {
  res.setHeader("Access-Control-Allow-Credentials", "true");
  res.setHeader(
    "Access-Control-Allow-Origin",
    "https://studio.apollographql.com"
  );
  res.setHeader(
    "Access-Control-Allow-Headers",
    "Origin, X-Requested-With, Content-Type, Accept"
  );
  if (req.method === "OPTIONS") {
    res.end();
    return false;
  }

  await startServer;
  await apolloServer.createHandler({
    path: "/api/graphql",
  })(req, res);
}

export const config = {
  api: {
    bodyParser: false,
  },
};

```
Aquí tenemos un ejemplo que a veces los ejemplos que vienen en NextJs a veces no funcionan porque se basan en versiones antiguas y cuya configuración ha variado. Yo me base en el siguiente [ejemplo de NextJS](https://github.com/vercel/next.js/tree/canary/examples/api-routes-apollo-server) donde se ve una aplicación de ejemplo para comprobar que estamos poniendo las cosas correctamente. Pero esta configuración no me funciona porque desde la versión 3 Apollo arranca su propio sandbox y la manera de arrancarse es distinta. En el ejemplo de Nextjs que he usado como base es [este](https://github.com/vercel/next.js/tree/canary/examples/api-routes-graphql) y es el que me ha funcionado.

Hay ejemplos de NextJS donde el schema y los resolvers los pone todo junto en el propio fichero de configuración de GraphQL, aunque en otros los tiene separado, yo opto por separarlos porque no hay que perder las buenas prácticas.

Si en consola arrancamos la aplicación:

```tpl
npm run dev
```

Arrancará bien sin problemas. Pero si accedemos a la siguiente URL:

```
http://localhost:3000/api/graphql
```

Nos va a dar un error porque los schemas no están configurados. Esta URL es la que nos va a permitir testear las operaciones configuradas en el schema. Para que nos funciones hay que configurar el schema.


# Configurando el schema

En la [documentación oficial de GraphQL](https://graphql.org/learn/schema/) podemos ver más en detalle las opciones que hay para configurarlo.

El schema que vamos a configurar va ser muy simple ya que se basa en la collection que hice en el [ejemplo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/) que cree para explicar el GUI de MongoDB.

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

En este schema he creado dos tipos, una query y un mutation. Para ver un ejemplo de su funcionamiento.

# Conectado MongoDB

La parte de configuración de la conexión con MongoDB se hace en este [artículo](https://irodrigob.github.io/docs/mongodb/usando_app/mongoose/) aparte porque es una configuración que sirve tanto para usarse en aplicación con GraphQL o sin el.

Lo que si que hay que añadir en el fichero *graphql.js* dentro de la ruta *pages->api* es lo siguiente sentencia:

```tpl
import "db/config";
```

La gracía de los import que solo se va ejecutar una vez aunque se llame varias veces, por eso se pone en el archivo que inicializa el servidor de Apollo.

# Probando en sandbox

Con el schema ya configurado y la aplicación de NextJS vamos a entrar en *http://localhost:3000/api/graphql* y veremos la siguiente pantalla:

![inicio sandbox](/images/graphql/servidor/sandbox_inicio.png)

Si le damos al botón *Query your server*:

![Schemas del sandbox](/images/graphql/servidor/sandbox_schemas.png)

Por defecto nos sale las querys pero si le damos a *Objects* podemos los tipos de datos que hemos definido.

Para poder probar las consultas y las mutations hay que implementar el resolver que lo haremos en un artículo aparte.



