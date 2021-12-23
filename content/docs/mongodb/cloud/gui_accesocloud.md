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

# Operacion CRUD

Si queremos ver todas las posibilidades de las operaciones CRUD lo mejor es ir a la [documentación oficial](https://docs.mongodb.com/v5.0/crud/), yo explicaré las opciones básicas.

## Inserción de documentos

En la siguiente [documentación oficial](https://docs.mongodb.com/v5.0/tutorial/insert-documents/) podemos ver más en detalle las posibilidades.

Para insertar un registro simple hacemos lo siguiente:

```tpl
use ejemplo

db.mitabla.insertOne({{mueble:"mesa", material:"madrea", medidas:{altura:50,anchura:30,unidad:"cm"})
```
El formato para introducir los registros es similar, o practicamente igual, al formato JSON.
Una vez ejecutado la sentencia veremos el siguiente resultado:

![Inserción registros](/images/mongodb/gui/robo_insercion_collection.png)

Es importante mencionar que siempre se inserta un campo llamado *_ID*. Este campo es que tendrá la clave única del registro. En la imagen lo podemos ver en la rama *insertedID*.

Si queremos insertar multiples registros a la vez podemos usar la opción *insertMany* poniendo un *[{<campos>}]*. Vamos un array de JSON. Ejemplo:

```tpl
use ejemplo

db.mitabla.insertMany([{mueble:"mesa", 
                        material:"madera", 
                        medidas:{altura:50,anchura:30,unidad:"cm"},
                        color:"blanco"},
                        {mueble:"silla", 
                        material:"madera", 
                        medidas:{altura:40,anchura:20,unidad:"cm"},
                        color:"gris"}])
```

El resultado en el log de la consola sería el siguiente:

![Inserción multiregistros](/images/mongodb/gui/robo_insercion_collection2.png)

De nuevo en la rama *insertedID* se pueden ver los *_ID* que se han insertado. 

Un detalle es que en esta inserción de varios documentos he añadido un campo nuevo, *COLOR* que no estaba en el primer registro que he añadido. Esto es posible en este tipo de base de datos, aunque personalmente no lo recomiendo porque entonces no hay manera de tener una uniformidad de campos en los registros.

Los datos también se pueden consultar de la collection desde la propia base del cloud del mongo:

![Consulta datos cloud](/images/mongodb/gui/robo_datos_collection_cloud.png)

En la imagen se ve claramente los campos insertados y el campo *_id* que es la clave única del registro.

## Consulta de documentos

En la [documentación oficial](https://docs.mongodb.com/v5.0/tutorial/query-documents/) encontraremos todas las opción disponibles.

### Buscar todos los documentos

La búsqueda de documentos es muy simple con la siguiente opción:

```tpl
use ejemplo

db.mitabla.find({})
```

El resultado sería el siguiente:

![Consulta todos los documentos](/images/mongodb/gui/robo_consulta_docs.png)


### Filtro por campos

El filtro de documentos es muy simple:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"})
```
El resultado:

![Filtro de documento](/images/mongodb/gui/robo_consulta_docs2.png)

Existe la posibilidad de usar operadores,"=,>=,etc", para poder hacer comparaciones. En la [documentación oficial](https://docs.mongodb.com/v5.0/reference/operator/query/#std-label-query-selectors) podemos ver los que hay y sus posibilidades.

Para poder filtrar por campos dentro de otros campos, como el que he usado en *medidas*, se hace de la siguiente manera:

```tpl
use ejemplo

db.mitabla.find({"medidas.anchura":30})
```

Y el resultado:

![Filtro de campos dentro de documentos](/images/mongodb/gui/robo_consulta_docs3.png)

### Proyección de campo o seleccionar que campos se quiere recuperar

En las consultas es posible indicar los campos que se requiere recuperar para ello justo después de poner los filtros se indicarán que campos se quieren visualizar.

Ejemplo:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"},{material:1})
```
El resultado:

![Campos en el filtro](/images/mongodb/gui/robo_consulta_docs4.png)

En la imagen se observa que se devuelven los campos de: *material* e *_id*. Si no queremos que el campo *_id* se devuelva hay que hacer lo siguiente:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"},{material:1,_id:0})
```
Para que no devuelva el campo hay que poner un *<nombre_campo>:0*. 

Ahora, si lo queremos ver el campo de *_id* habría que hacer:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"},{_id:1})
```
Esto hace que solo se muestre el campo *_id*. 

Si hacer la inversa, es edcir, que se muestren todos los campos menos el *_id*:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"},{_id:0})
```

Más información el [documentación oficial](https://docs.mongodb.com/v5.0/tutorial/project-fields-from-query-results/)

## Actualización de documentos

En la [documentación oficial](https://docs.mongodb.com/v5.0/tutorial/update-documents/) podremos obtener todas las opción en la actualización.

Ejemplo vamos actualizar a filtrar los documentos que son *mesa* para poner que el tipo de material es *madera*, hay u

```tpl
use ejemplo

db.mitabla.updateMany({mueble:"mesa"},
    {
     $set: { "material": "madera"},    
   })
```

Como tenemos varios registro que son *mesa* he usado la opción *updateMany*. Si solo queremos actualizar un solo registro se usará la opción *updateOne*.

Si volvemos a lanzar la consulta:

```tpl
use ejemplo

db.mitabla.find({mueble:"mesa"},{mueble:1,material:1})
```
Veremos los registros actualizados:

![Campos en el filtro](/images/mongodb/gui/robo_update_collection.png)