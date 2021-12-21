---
title: GUI para acceder al cloud de mongo DB
description: Programa GUI para acceder al cloud de mongo DB
---

# Introducción

Supongo que habrá muchos GUI para poder interacturar con MongoDB pero conoci [Robo](https://robomongo.org/) en un video para aprender como funciona. La versión que tengo instalada es la *Robo 3T* que es gratuíta que para lo que necesito es suficiente.

# Conexión con la base de datos

Una vez se entra al Robo 3T nos sale esta ventana:

![Robo connection](/images/mongodb/gui/robo_connection.png)

Para crear una nueva conexión hay que pulsar sobre el enlace *Create*

![Robo connection settings](/images/mongodb/gui/robo_connection_settings.png)

Aquí entra en juego el string de conexión:

```tpl
mongodb+srv://<usuaurio>:<password>@cluster0.6huz7.mongodb.net/myFirstDatabase?retryWrites=true&w=majority
```

Que nos daba el botón *Connect* de nuestro base de datos.

Este string una vez puesto usuario, password y base de datos correctos hay que ponerlo en el campo al lado del botón "From URI" y a continuación pulsar dicho botón. Nos aparecerá arriba los servidores:

![Robo connection settings 2](/images/mongodb/gui/robo_connection_settings.png)

Salen tres servidores porque aún en la versión gratuita hay replicadas. El paso final es darle un nombre y darle al botón *Save* y nos saldrá las conexiones:

![Robo connection](/images/mongodb/gui/robo_connection2.png)

Al hacer doble click nos conectaremos a nuestra base de datos, apareciendonos a la derecha los datos de la conexión:

![Lista conexiones](/images/mongodb/gui/robo_lista_conexiones.png)

Para activar el shell para empezar a trabajar hay que hacer doble click sobre un nodo que aparecerá en la carpeta *Replica set*, al hacer nos saldrá a la derecha lo siguiente:

![Shell inicial de la connection](/images/mongodb/gui/robo_connection_shell.png)

**IMPORTANTE: Cuando se escriba los comandos en el shell para que se ejecuten hay que pulsar CTRL+ENTER. Si se pulsar solo ENTER solo hará un salto de línea.**

# Creación de la base de datos y collection

Para crear o usar una de datos hay que usar el comando *use*. Ejemplo:

```tpl
use ejemplo
```
En el shell nos dirá que se ha cambiado la base de datos.

![Creacion/cambio base de datos](/images/mongodb/gui/robo_command_shell_1.png)

El mensaje será el mismo tanto si la base de datos existe como si no. Si la base de datos no existe no se creará hasta que no se cree una *collection*.

**IMPORTANTE:** A partir de ahora todos los comados para hacer cualquier operación con la base de datos tiene que empezar por *DB*.

Para crear la collection se puede hacer de dos maneras: 

1. Insertar directamente un valor:

```tpl
db.myNewCollection2.insertOne( { x: 1 } )
```

2. Creando directamente el collection, que es como me gusta más.

```tpl
db.createCollection('mitabla') 
```
Una ejecutado la sentencia nos aparecerá una ventana debajo:

![Creando una collection](/images/mongodb/gui/robo_command_shell_2.png)

**Importante:** Si no se pone el *use* la collection se creará en la base de datos por defecto que es *Cluster0*. Por ello el use hay que dejarlo siempre.

Donde vemos la definición de la *collection* creada. Lo que sale no lo puedo llamar campos porque después veremos que puedes insertar cualquier tipo de datos. Vamos que un registro que añadas y otro pueden ser completamente distintos, lo malo que cuando se consulte no es viable tener estructuras distintas, pero realmente se puede hacer.

La ventana que sale yo la desacoplo y lo reduzco y lo pongo a un lado para seguir viendo el shell. 

Una vez creada la collection la base de datos se crea, si no lo estuviera previamente. Eso lo podemos ver en la propia web:

![Collection creada en el cloud](/images/mongodb/gui/robo_collection_creada_cloud.png)

# Añadiendo registros a la collection

Si queremos ver todas las posibilidades de las operaciones CRUD lo mejor es ir a la documentación [oficial](https://docs.mongodb.com/v5.0/crud/), yo explicaré las opciones básicas.