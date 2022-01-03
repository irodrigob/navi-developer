---
title: Resolvers
description: Resolvers
---

# Introducción

Los resolver son los ejecutores de lo que se ha definido en los [schemas](https://irodrigob.github.io/docs/graph_ql/schemas/).

Aunque para estos ejemplos se podría haber puesto todo el código en el mismo fichero que el *schema* pero lo correcto es tenerlo separado por carpetas:

```tpl
|- pages
|--- api
|----- graphql.js
|----- resolvers
|------- index.js
|----- schemas
|------- index.js
```

