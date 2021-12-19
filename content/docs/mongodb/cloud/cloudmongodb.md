---
title: cloud.mongdb
description: cloud.mongdb
---

# Introducción

La página oficial de [mongodb](https://www.mongodb.com/) tiene su propia versión [cloud](https://cloud.mongodb.com/) para poder desarrollar.

Te puedes loguearte con tu mails o usar con tu cuenta de google. En mi caso he usado la cuenta de google, es más comodo.

# Plannes y precios

Hay varias modalidades y precios. Cuando entras por primera vez te sale que es lo que quieres hacer. Te saldrá una página como esta:

![Planes y precios](/images/mongodb/cloudmongodb/planesyprecios.png)

Esta página es la que te saldrá si quieres crearte un nuevo cluster.

# Opción gratuita

El plan que uso es el que se demonia *Shared*, que es la ideal para crear proyectos personales o para ir aprendiendo. 

La versión gratuita solo puedes tener un cluster por proyecto, no se tiene la última versión de mongodb y no es escalable. Pero para temas personales es irrelevante.

# Cluster inicial.

Cuando se crea por primera vez la versión grauita te crea por defecto el *Project 0* con el *cluster0*:

![proyecto y cluster inicial](/images/mongodb/cloudmongodb/proyectoclusterinicial.png)

## Creación de usuarios

Ahora hay que crear un usuario para poder conectarnos a la base de datos. La primera vez como hay un asistente te indicaré como crearlo. Pero si quieres crear otros usuarios en el menú de la derechaz tenemos la opción *Database Access*:

![Database access](/images/mongodb/cloudmongodb/databasaccess.png)

Este primer usuario tendrá permisos administrador y es el que usaremos para podernos conectarnos a nuestro proyecto.

## Registringir por IP

En el asistente de creación inicial un paso es limitar el acceso por IP, en el asistente no hay botón que permita indicarle que no registra por IP. Yo me salte ese paso y lo he hecho a mano.

Justo debajo donde hemos creado los permisos de usuario tenemos la opción *Network access*:

![Network access](/images/mongodb/cloudmongodb/networkaccess.png)

Al pulsar el botón *Add IP Address* saldra esta ventana:

![Add ip access](/images/mongodb/cloudmongodb/add_ip_access.png)

Aquí se le pulsa al botón *Allow access from anywhere* y confirmar. Con esto ya tenemos que cualquier pueda acceder a nuestra base de datos.

## Conexión con aplicaciones

El siguiente paso es saber como conectarnos desde nuestra aplicación. Para ello hay que pulsar el botón *Connect* y aparecerá esta pantalla:

![Connect](/images/mongodb/cloudmongodb/connectcluster.png)

En esta ventana escogeremos la opción *Connect your application* y nos aparecerá lo siguiente:

![Connect your application](/images/mongodb/cloudmongodb/connectyouapplication.png)

Yo he escogido la opción nodejs porque lo voy utilizar en una aplicación de Nextjs. 

Aquí lo importante es el string que devuelve:

```tpl
mongodb+srv://<usuaurio>:<password>@cluster0.6huz7.mongodb.net/myFirstDatabase?retryWrites=true&w=majority
```

Donde pone *usuario* es el usuario que hemos creado en la opción *Database access*. En *password* el password que le hemos puesto. Y donde pone *myFirstDatabase* hay que poner el nombre de la base de datos, que en este caso se llamará *Cluster0*.

Con esto ya podemos conectarnos con nuestra aplicación javascript o cualquier otro programa, como veremos, que permite conectarse a dicha base de datos.

## Accediendo a la base de datos

En la pantalla inicial de nuestro proyecto si pulsamos sobre *Cluster0* se navegará al detalle de la base de datos o cluster, o como se llame. Porque digo eso porque si vamos a la pestaña *Collections* vemos lo siguiente:

![Collections database](/images/mongodb/cloudmongodb/tab_collections_database.png).

Aquí vemos que tengo una base de datos llamada *transporttooldb* que dentro de ella tiene un *collections*. Un collection sería una tabla en las bases de datos relacionales.

Mi confusión es porque aunque hay un botón que se llama *+Create database* yo creo que es más bien para crear un namespace o algo parecido.





