---
title: Estilos
description: Estilos
---

# Introducción

Aquí viene los estilos o CSS que tanto me dan por saco. Recopilización de trucos o utilidades que he usado y no quiero perderlas.

# Combinar dos clases en un mismo *className*

Tenia una necesidad de poner en un mismo *className* dos estilos distintos, porque no sabía, ni sé, como crear un estilo que herede de otro o combinarlos. Todo esto para no tener que duplicar estilos.

Total, que al final tengo un estilo global y otro a nivel local la solución: Estilos

Primero hay que importar una librería, creo que es propia de Mater:
```tpl
import classNames from "classnames";
```

Luego importaba mis estilos:
```tpl
import GlobalStyle from "src/globalStyle";
import LocalStyle from "components/invoiceTraining/imageOCRDataCSS";
```

Luego poner en un objeto los datos estilos:
```tpl
const useStyles = GlobalStyle;
const useLocalStyles = LocalStyle;
```

Luego en las variables del componente declaro los estilos que usará en el componente: 
```tpl
  const classesGlobal = useStyles();
  const classesLocal = useLocalStyles();
```

Y ahora, en el componente gráfico:

```tpl
<FaAngleDown
   className={classNames(
                classesGlobal.icon,
                classesLocal.icon
             )}
/>
```

