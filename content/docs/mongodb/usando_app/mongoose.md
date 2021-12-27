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
    process.exit(1);
  }
};
```