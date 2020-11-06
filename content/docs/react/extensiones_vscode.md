---
title: Extensiones para VS Code
description: Extensiones para VSCode que uso React
---

# Introducción

El objetivo es poner la extensiones que uso en VSCode para desarrollar React, aunque alguna de ellas son multilenguaje.

# Error Lens

Esta extensión muestra los mensaje de error, warning, informativos, etc.. al lado de la línea de código. Es muy practico ya que no hay que mirar en la pestaña de *Problems* para saber que errores tenemos.

Más info en este [enlace](https://marketplace.visualstudio.com/items?itemName=usernamehw.errorlens) del market place.

# Next.js snippets

Aunque es para next.js, hay otra parecida para React, esta xtensión abilita un conjunto de macros que simplifican a la hora de introducidar sentencias como funciones, componentes, etc..

La verdad es que no desarrollo tan habitual como quería se me olvidan las macros, o las tengo que tener un notepad los atajos, lo que no lo hace muy práctico.

# Prettier - code formatter

Es la extensión que uso para que formatear el código. Creo que para este extensión tuve que tocar directamente la configuración del VSCode para asociarla el lenguaje a la extensión. Ahora mismo lo tengo así:

```tpl
"[html]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
},
"[css]": {
  "editor.defaultFormatter": "esbenp.prettier-vscode"
},
"[javascript]": {
  "editor.defaultFormatter": "esbenp.prettier-vscode"
}
```
