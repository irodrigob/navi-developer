---
title: Utilizar variables de entorno
description: Utilizar variables de entorno
---

# Funcionamiento

Las variables de entorno permite tener una configuración a nivel de servidor. Que quiero decir, que son variables que no podrán ser uasadas en lado cliente. Para que las uso yo: Definir URL de conexión a una base de datos, usuarios y password. No soy ningún experto pero pongo lo que he visto y probado.

Las variables se leen cuando se inicia el proyecto, por lo tanto, cualquier cambio que se haga habrá que parar el proyecto e iniciarlo.

Hay dos tipos de variables de entorno:

* El que empieza por *.env.local* este fichero es ideal para poner usuarios y contraseñas porque no se sube al git.
* El que empieza por *.env* lo uso para indicar el entorno porque este, si que sube al git.

Sobre indicar el entorno según NextJS te indica que se use el parámetro *NODE_ENV* pero a mi no me termina de funcionar. Uso una variable propia llamada *ENVIRONMENT* y es donde indica el entonro.

