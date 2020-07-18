---
title: BAPIs
description: BAPIs
bookCollapseSection: true
---

# Introducción

Las BAPIs son objetos de negocio (pedidos, documentos financieros, empleado, etc.) que aglutinan un conjunto de funciones que permiten crear, modificar o consultar datos de negocio. Por ejemplo, el el objeto de negocio de pedido de compras tendríamos las opciones de: crear. modificar, liberar, etc..

Esas funcionalidad son funciones, de tipo RFC, que permiten crear desde pedidos, contabilizar documentos financieros, etc. Por eso coloquialmente cuando se habla de BAPI se refiere a una función, porque dichas funciones normalmente comiezan por *BAPI_*.

La transacción donde podemos ver los objetos de negocio es la transacción *BAPI*. Aunque muchas veces hay funciones que no están inventaridas en dicha transacción, por ello, para funciones de módulos pequeños o acciones muy específicas lo mejor es buscar las funciones que comiencen por *BAPI_*.

# Ejemplos de BAPIs

Los ejemplos de BAPIs se van a ir organizando por módulos para que quede mejor organizado.

{{<section>}}