---
title: Mongoose
description: Conector Mongoose
---

# Introducción

[Mongoose](https://mongoosejs.com/) es un conector que permite conectar nuestras aplicación con MongoDB. 

# Instalando Mongoose en una aplicación NodeJS

Este ejemplo es para poder conectar una aplicación hecha en NextJS con la versión MongoDB cloud. 

Para poder usar mongoDB en una aplicación hay que instalar mediante:

```tpl
npm install mongoose
```

Una vez instalado he creado un carpeta en el raíz del proyecto llamada *db* donde he creado el archivo *db.js* donde he añadido lo siguiente:

```tpl
import mongoose from "mongoose";

const MongoDb = process.env.MONGODB_URI;

mongoose
  .connect(MongoDb, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useFindAndModify: false,
    useCreateIndex: true,
  })
  .then(() => {
    console.log("db success connect");
  })
  .catch((error) => {
    console.log("error connecting to database: ");
    console.log(err);
  });
```
Para el ejemplo voy a conectarme a la versión cloud que tiene MongoDB, en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/) se da más detalle de como hacerlo. Uso una variable de entorno que contiene la URL con el usuario y contraseña para que no sea visible en el códigp. Como se construye la URL y como se obtiene esta explicada en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/).

En este archivo no hago ningún export del código porque lo que haré es importarlo directamente. El motivo es que el import solo se ejecuta una vez aunque lo llamaes varias veces. De esta manera cuando se importe en el código la conexión se llamará solo una vez.

# Creando el modelo

El siguiente paso es crear los modelos o schema. Los schema se asigna a una colección y define como se van a comportar los documentos en la colección. En la [documentación oficial](https://mongoosejs.com/docs/guide.html#schemas) tenemos más información de como funcionan y sus opciones.

Siguiendo la collection que cree en este [artículo donde hablo como acceder vía GUI al modelo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/) he creado el siguiente modelo:

```tpl
import mongoose from "mongoose";
import uniqueValidator from "mongoose-unique-validator";

const miTablaSchema = new mongoose.Schema({
  name: {
    type: String,
    required: true,
    unique: true,
  },
  material: {
    type: String,
  },
  medidas: {
    type: String,
  },
});

miTablaSchema.index({ name: "name" });
miTablaSchema.plugin(uniqueValidator);

export default mongoose.model("MiTabla", miTabla);

```

En la colección he definido los siguientes campos:

* name -> Que será será de tipo *String* y como es un campo importe le he indicado que sea obligatorio y con valor único. 
* material y medidas que será de tipo *String*.

He añadido un plugin que da más detalle, o da mejor detalle según he leído, cuando hay duplicados cuando detecta que un campo tiene el atributo *unique: true*.

Debido a que hay que usar *export default* hay que crear un fichero por modelo.

# Usando la base de datos

Como voy a usar MongoDB para GraphQL todo eso proceso estará explicando en los artículos GraphQL.
