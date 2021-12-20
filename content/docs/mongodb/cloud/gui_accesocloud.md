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

# Creación de la base de datos y collection



