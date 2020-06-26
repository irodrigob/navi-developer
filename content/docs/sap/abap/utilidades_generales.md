---
title: Utilidades generales
description: Utilidades generales
bookCollapseSection: true
---

# Objetivo

Cajón de desastre de cosas que no se muy como clasificar.

# Hacks

## Saltarse seguridad como desarrollador

Para saltarse la seguridad del usuario desarrollador

 ![Saltarse seguridad](/images/hugo/publicar_web/util_general_saltarse_seguridad_desarrollador.png)

 # URL

 * http://wiki.sdn.sap.com/wiki/display/Basis/Timezone+changes+best+practices --> Best practices sobre los husos horarios

 # Rellenar campos con 0

 ```tpl
data: l_char(50).

TRANSLATE l_char USING ' 0'.
WRITE l_char. 
```
