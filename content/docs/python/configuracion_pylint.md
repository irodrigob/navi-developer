---
title: Configuración Pylint
description: Configuración Pylint
---

# Introducción

Pylint es una librería que se usa para verificiar que el código de Python que estamos escribiendo, para el entorno que tengamos activado, sea correcto.

Pero hay veces que hay que tocar la configuración de Pylint para afinarlo.

# Evitar errores de librerías que no existen

Hay casos que Pylint dice que una librería no existe pero realmente si que existe. En mi caso me ha ocurrido al usar librerías creadas en el raíz del proyecto de VS Code, dentro de un proyecto de Django. Pero luego, en tiempo de ejecución no da problemas.

El motivo es que esa librería propia no esta en las variables de entorno y esto provoca que Pylint piense que no existe.

La solución es modificar el fichero *.pylintrc* que esta en el raíz del proyecto de VS Code. Si no existiera se puede crear mediante el siguiente comando:

```
pylint --generate-rcfile > .pylintrc
```

Una vez en el archivo hay que localizar la variable *init-hook* y añadir lo siguiente:
```
init-hook='import sys; sys.path.append("K:\github\ocr_invoices")'
```

Donde se le añade el directorio del proyecto a las variables de entorno. De esta manera desaparecen ese tipo de errores.

