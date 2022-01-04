---
title: Probando servidor GraphQL
description: Probando el servidor GraphQL
---

# Introducción

Una vez se tenga el [schema](https://irodrigob.github.io/docs/graph_ql/schemas/) definido, el [resolver](https://irodrigob.github.io/docs/graph_ql/resolvers/), y el [servidor](https://irodrigob.github.io/docs/graph_ql/servidor_graphql/) configurado y conectado con [MongoDB](https://irodrigob.github.io/docs/mongodb/usando_app/mongoose/) es momento de probarlo. En el artículo donde se explica como configurar el servidor se indica que para acceder al sandbox para probar el servidor hay que ir a la siguiente URL:

```
http://localhost:3000/api/graphql
```

El puerto 3000 es porque estamos usando NextJS para las pruebas.

# Estado inicial MongoDB

La foto inicial que tenemos en MongoDB es la siguiente:

![mongodb estado inicial](/images/graphql/servidor/mongo_estado_inicial.png)

No hay nada creado.

# Realizando la primera consulta

Desde Apollo studio te guarda lo último que has hecho y ejecutado pero creo que la primera te sale directamente en la operaciones de *Query*, por ello para no liarse lo mejor es desmarca todo y tener esta imagen inicial:

![Operaciones iniciales](/images/graphql/servidor/sandbox_operacion_inicial.png)

No voy a entrar mucho en detalle porque la página es bastante intuitiva pero para los avezados pueden escribir directamente las consultas en la *Operations*, pero yo uso el asistente. El asistente consiste en utilizar este icono ![icono añador](/images/graphql/servidor/icono_anyadir.png) para ir añadiendo campos o las operaciones que deseamos. En este caso vamos a darle al icono al lado de *Query* con lo que nos aparecerá lo siguiente:

![Operacion query seleccionada](/images/graphql/servidor/operacion_query_seleccionada.png)

Ahora si damos al icono de añadir al lado de *getAll* se nos añadirá la opción:

![Operacion query seleccionada2](/images/graphql/servidor/operacion_query_seleccionada2.png)

Ahora solo falta añadir los campos de nuevo con el icono de añadir. Donde nos quedará lo siguiente:

![Operacion query seleccionada3](/images/graphql/servidor/operacion_query_seleccionada3.png)

El paso final es ejecutar la query: ![Ejecutar query](/images/graphql/servidor/boton_ejecutar_query.png)

![Operacion query seleccionada4](/images/graphql/servidor/operacion_query_seleccionada4.png)

# Añadiendo datos

Vamos aprovechar que se pueden crear pestaña con este icono: ![Añadir pestaña](/images/graphql/servidor/boton_anyadir_pestanya.png) para añadir la mutation *newProduct*:

![Operacion mutation](/images/graphql/servidor/operacion_mutation1.png)

Por defecto en los parámetros nos ha colocado el campo *mueble* porque en el *schema* lo hemos definido como obligatorio.  En el panel de la izquierda tenemos dos secciones: *Arguments* y *Fields*. La primera son los parámetros de entrada del mutation, y el segundo son los campos de salida una vez el registro se haya insertado (es como lo que hemos hecho en la query). Si añadimos todos los campos necesarios tenemos lo siguiente:

![Operacion mutation2](/images/graphql/servidor/operacion_mutation2.png)

Ahora falta añadir los valores. Los valores se pueden añadir, o bien, en la pestaña de *Variables*, o bien, en la propia consulta.  Lo vamos hacer de las dos maneras para verlas. La primera es través de las *Variables*

```tpl
{
  "mueble": "mesa",
  "material": "madera"
}
```

Al pulsar el botón de ejecutar ![Boton ejecutar](/images/graphql/servidor/boton_ejecutar_mutation.png) tendremos el siguiente resultado:


![Operacion mutation3](/images/graphql/servidor/operacion_mutation3.png)

Si lo hacemos directamente en la mutation lo que hago es quitar el contenido en la pestaña de *Variables* y poner lo siguiente en la propia consulta:

```tpl
mutation Mutation($mueble: String!, $material: String) {
  newProduct(mueble: "silla", material: "plastico") {
    mueble
    material
  }
}
```
Al ejecutar la consulta obtendremos el siguiente resultado:

![Operacion mutation4](/images/graphql/servidor/operacion_mutation4.png)

Ahora, si volvemos a la pestaña donde tenemos la consulta y la ejecutamos veremos los dos registros insertados:

![Operacion query seleccionada5](/images/graphql/servidor/operacion_query_seleccionada5.png)

