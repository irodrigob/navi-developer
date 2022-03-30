---
title: Información general
description: Información general
---

# Objetivo

Esta página contendrá información de general sobre BW que no se muy bien donde clasificar.

# Como probar una query en R/3

Normalmente las querys se suelen utilizar desde herramientas de SAP que están fuera del SAP GUI. Pero puede ser interesante, poder probar dicha query para ver que datos tienes y que se puede hacer con él. El siguiente ejemplo de como hacerlo se basa en una query que usa un cubo generado con un CDS desde un sistema S/4 HANA, pero puede servir para los cubos tradicionales de BW.

Lo primer es ir a la transacción *RSRT*. En el campo de query hay que pulsar sobre la ayuda para búsqueda y buscar el cubo:

![Seleccion cubo](/images/sap/BW/info_general_seleccion_cubo.png)

En este caso hemos ido a la pestaña de *InfoÁreas* y en nodos no asignados. Hemos hecho doble click en el nodo seleccionado y pulsaremos el botón *Ejecutar* para lanzar la query:

![Seleccion cubo](/images/sap/BW/info_general_pantalla_sel_cubo.png)

Aparecerá una pantalla para introducir los datos de selección. Y una vez introducido se pulsará el siguiente botón ![Confirmar selección](/images/sap/BW/info_general_pantalla_sel_cubo_confirmar.png) y se ejecutará la consulta:

![Salida del cubo](/images/sap/BW/info_general_salida_cubo.png)

Los datos que aquí se muestran serán los mismos que salgan en cualquier heramienta que explote dicho cubo.

# Variables del query designer

En el query designer para poder hacer filtros variables, es decir que se pasan los datos a filtrar por parámetro, hay que crear variable y asignarsela al campo. Pues una vez se crea la variable no es posible editarla o borrar, la única manera de hacerlo es a través de la tabla *RSZGLOBV*. En esta tabla filtrando por el campo *VNAM* por la variable obtenemos los datos de la variable, se podrá borrar o modificar la configuración de las variables.

# Como llamar a Querys creadas en el Query Designer

En mi github he creado un [repositorio de utilidades de BW](https://github.com/irodrigob/ABAP_BW_UTILS). En este repositorio esta la clase *ZCL_CA_BW_QUERY_BEX* que encapsula las llamadas de querys creados en el Query Designer.

