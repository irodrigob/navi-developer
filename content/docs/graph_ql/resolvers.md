---
title: Resolvers
description: Resolvers
---

# Introducción

Los resolver son los ejecutores de lo que se ha definido en los [schemas](https://irodrigob.github.io/docs/graph_ql/schemas/). En el resolver es donde se llama al sistema donde obtendremos, o actualizaremos, los datos que necesitemos, en nuestro caso la conexión con MongoDB. La potencia que tiene graphQL que es el sistema donde están los datos es irrelevantes porque al final se puede conectar a donde sea, como si es un fichero plano. Es más, se podría obtener la información desde dos, o más, sistemas distintos.

Aunque para estos ejemplos se podría haber puesto todo el código en el mismo fichero que el *schema* y el fichero *graphql.js*, conexión con el servidor, pero lo correcto es tenerlo separado por carpetas tal como ya se explico en otro artículo:

```tpl
|- pages
|--- api
|----- graphql.js
|----- resolvers
|------- index.js
|----- schemas
|------- index.js
```

Siguiendo el schema indicado en la [introducción de este artículo](https://irodrigob.github.io/docs/graph_ql/schemas/#introducci%c3%b3n) se ha definido el siguiente resolver:

```tpl
import Product from "db/models/product.js";

export const resolvers = {
  Query: {
    // products
    getAll: async (root, args) => {
      return await Product.find({});
    },
  },
  Mutation: {
    newProduct: (root, args) => {
      const row = new Product({ ...args });
      return row.save();
    },
  },
};
```

En este resolver lo primero es importar el modelo de Mongoose para poder hacer las peticiones. La estructura del resolver es prácticamente la misma que la que se indica en el *schema*, la diferencia que aquí no hay tipos. 
El caso de las querys son funciones *async* para permitir esperarse al resultado del proceso.

En cualquier caso, en todas las funciones tienen que devolver algo. Ese algo será un objeto que GraphQL mapeará a los campos del schema definido. Por ello un punto es que los campos del schema y del sistema al cual se accede tienen que tener los mismos nombres.

Otro detalle es que en las funciones tienen dos argumentos:

* root -> Son los datos de la función padre. Esto se usa cuando vas construyendo consultas anidadas o quieres quieres formatear campos a partir de los obtenidos. Tengo pendiente de un ejemplo para ver como funciona.
* args -> Argumentos de la función. Esto va ligado con el parámetro *root*, ya que creo que se pasan los datos de la función superior, y también va ligado en las mutation porque son los parámetros que se definen en los schemas.


