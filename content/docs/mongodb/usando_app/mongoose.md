---
title: Conector Mongoose
description: Conector Mongoose para MongoDB
---

# Introducción

[Mongoose](https://mongoosejs.com/) es un conector que permite conectar nuestras aplicación con MongoDB. 

# Instalando Mongoose en una aplicación NodeJS

Este ejemplo es para poder conectar una aplicación hecha en NextJS con la versión MongoDB cloud. 

Para poder usar mongoDB en una aplicación hay que instalar mediante:

```tpl
npm install mongoose
```

Una vez instalado he creado la siguiente estructura de carpetas:

```tpl
|- db
|--- config
|----- index.js
|--- models
|----- Product.js
```

En la carpeta *config* es donde tendremos la conexión con la base de datos:

```tpl
import mongoose from "mongoose";

const MONGODB_URI = process.env.MONGODB_URI;

if (!MONGODB_URI) {
  throw new Error(
    "Please define the MONGODB_URI environment variable inside .env.local"
  );
}

mongoose
  .connect(MONGODB_URI, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
  })
  .then(() => {
    console.log("db success connect");
  })
  .catch((error) => {
    console.log("error connecting to database: ");
    console.log(error.message);
  });
```
Para el ejemplo voy a conectarme a la versión cloud que tiene MongoDB, en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/) se da más detalle de como hacerlo. En el ráiz del proyecto he creado el fichero *.env.local* donde tengo la url con el usuario y password de conexión.  Como se construye la URL y como se obtiene esta explicada en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/).

En este archivo no hago ningún export del código porque lo que haré es importarlo directamente. El motivo es que el import solo se ejecuta una vez aunque lo llamaes varias veces. De esta manera cuando se importe en el código la conexión se llamará solo una vez.

En los ejemplos que he visto hay dos parámetros que se ponen pero en mi caso no ha funcionado, supongo que por versión de la base de datos en el cloud:

```
useFindAndModify: false,
useCreateIndex: true,
```    

# Creando el modelo

El siguiente paso es crear los modelos o collections. Los modelos definen que campos y como se van a comportar en los documentos en la colección. En la [documentación oficial](https://mongoosejs.com/docs/guide.html#schemas) tenemos más información de como funcionan y sus opciones. 

Si en los ejemplos que puse en este [artículo donde hablo como acceder vía GUI al modelo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/), ponía cualquier nombre como ejemplo, en este caso y para que funcione hay que tener en cuenta que el nombre del modelo será en singula y la primera letra en mayúsculas. El nombre que le daremos será *Product* cuando se ejecute el modelo en la base de datos se creará como *products*. Mongo lo pone a minúsculas y lo pone en plurar.

```tpl
import mongoose from "mongoose";
import uniqueValidator from "mongoose-unique-validator";

mongoose.Promise = global.Promise;

const ProductSchema = new mongoose.Schema({
  mueble: {
    type: String,
    required: true,
    unique: true,
  },
  material: {
    type: String,
  },
  medidas: {
    altura: Number,
    anchura: Number,
    unidad: String,
  },
});

ProductSchema.index({ name: "mueble" });
ProductSchema.plugin(uniqueValidator);

let Product =
  mongoose.models.Product || mongoose.model("Product", ProductSchema);
export default Product;

```

En la colección he definido los siguientes campos de dos maneras posibles.

* name -> Que será será de tipo *String* y como es un campo importe le he indicado que sea obligatorio y con valor único. 
* material -> Es un campo string.
* medidas -> Es un campo estructurado, es decir, un campo que puede tener otro documento asociado. Esto en una base de datos relacional habría que crear dos tablas y relacionarlas, en MongoDB, al ser no-SQL, podemos poner otro documento en un campo. Podemos ir tantos niveles com queramos.
  * Dentro de medidas tenemos tres campos que los definimos directamente sin necesidad de abrir *{}* para indicar los atributos del campo.



He añadido un plugin que da más detalle, o da mejor detalle según he leído, cuando hay duplicados cuando detecta que un campo tiene el atributo *unique: true*.

Para evitar el error: *OverwriteModelError: Cannot overwrite `Product` model once compiled* pone en la definición de la variable *Product* que si el modelo ha sido creado previamente devuelva el método y no lo vuelve a crear.

Como hay que usar *export default* hay que crear un fichero por modelo.

# Usando la base de datos

Como voy a usar MongoDB para GraphQL todo eso proceso estará explicando en los artículos GraphQL.
