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

export const connectDb = async () => {
  // La configuración es una por defecto que he visto que evita que salte warnings.
  try {
    await mongoose.connect(MongoDb, {
      useNewUrlParser: true,
      useUnifiedTopology: true,
      useFindAndModify: false,
      useCreateIndex: true,
    });
    console.log("db success connect");
  } catch (err) {
    console.log("error connecting to database");
    console.log(err);    
  }
};
```
Para el ejemplo voy a conectarme a la versión cloud que tiene MongoDB, en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/) se da más detalle de como hacerlo, por ello la URI la tengo en una variable de entorno ya que contiene usuario y contraseña. Pero la URL que se usa esta explicada en este [artículo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/).

# Creando el modelo

El siguiente paso es crear los modelos o schema. Los schema se asigna a una colección y define como se van a comportar los documentos en la colección. En la [documentación oficial](https://mongoosejs.com/docs/guide.html#schemas) tenemos más información de como funcionan y sus opciones.

Siguiendo la collection que cree en este [artículo donde hablo como acceder vía GUI al modelo](https://irodrigob.github.io/docs/mongodb/cloud/gui_accesocloud/) he creado el siguiente modelo:

```tpl
import mongoose from "mongoose";

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

export default mongoose.model("MiTabla", miTabla);
```

En la colección he definido los siguientes campos:

* name -> Que será será de tipo *String* y como es un campo importe le he indicado que sea obligatorio y con valor único. 
* material y medidas que será de tipo *String*.

Debido a que hay que usar *export default* hay que crear un fichero por modelo.

# Usando la base de datos

Como voy a usar MongoDB para GraphQL todo eso proceso estará explicando en los artículos GraphQL.
