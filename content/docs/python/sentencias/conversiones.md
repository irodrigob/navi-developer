---
title: Conversiones
description: Conversiones
---

# De un objeto a string

Con la sentencia *str* se puede convertir de un objeto a un string. Ejemplo:

```tpl
id = uuid.uuid1()
filename = str(id)+"_"+secure_filename(file.filename)
```
