---
title: Información general
description: Información general
---

# Objetivo

Esta página contendrá información de general sobre BW que no se muy bien donde clasificar.

# Como probar un cubo en R/3

Normalmente los cubos se suelen utilizar desde herramientas de SAP que están fuera del SAP GUI. Pero puede ser interesante, poder probar el cubo para ver que datos tienes y que se puede hacer con él. El siguiente ejemplo de como hacerlo se basa en cubo generado con un CDS desde un sistema S/4 HANA. 

Lo primer es ir a la transacción *RSRT*. En el campo de query hay que pulsar sobre la ayuda para búsqueda y buscar el cubo:

![Seleccion cubo](/images/sap/BW/info_general_seleccion_cubo.png)

En este caso hemos ido a la pestaña de *InfoÁreas* y en nodos no asignados. Hemos hecho doble click en el nodo seleccionado y pulsaremos el botón *Ejecutar* para lanzar la query:

![Seleccion cubo](/images/sap/BW/info_general_pantalla_sel_cubo.png)

Aparecerá una pantalla para introducir los datos de selección. Y una vez introducido se pulsará el siguiente botón ![Confirmar selección](/images/sap/BW/info_general_pantalla_sel_cubo_confirmar.png) y se ejecutará la consulta:

![Salida del cubo](/images/sap/BW/info_general_salida_cubo.png)

Los datos que aquí se muestran serán los mismos que salgan en cualquier heramienta que explote dicho cubo.