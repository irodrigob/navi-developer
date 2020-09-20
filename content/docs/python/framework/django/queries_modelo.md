---
title: Queries en modelo de datos
description: Queries en modelo de datos
---

# Introducción

Es poner información de como realizar consultas al modelo de datos. La [documentación oficial de la versión 3.1](https://docs.djangoproject.com/en/3.1/topics/db/queries/) esta bien pero alguna cosa he tenido que buscar por internet. Así que, aquí recojo todo lo que he ido haciendo.

Los ejemplos sirven para cualquier modelo ya que el funcionamiento es igual para todo tipo de modelo, ya sea propio de Django o creado por nosotros.

# Recuperar todos los registros

En este ejemplo se basa en recuperar todos los permisos globales configurados.

```tpl

from django.contrib.auth import models

permisos_global = models.Permission.objects.all()
for permiso in permisos_global:
    print(permiso.codename)
```

Recuperar todos los datos siempre es con _.objects.all_. Luego se recorre cada registro con un _for_.

# Fitrando por un patron

Hay varias comandos para buscar por patrones.

## En cualquier parte del campo

Este ejemplo es el equivalente a este select: _SELECT ... FROM modelo WHERE campo LIKE '%OCR%'_.

```tpl
from django.contrib.auth import models

permisos_filtros = models.Permission.objects.filter(
            codename__contains='OCR')

for permiso in permisos_filtros:
            print("Permiso filtrado: ", permiso)

```

Cuando se quiere filtrar por patron hay que poner _\_\_contains_ al nombre de campo e indicar el valor, sin poner _%_.

## Al inicio del campo

Si tomamos el ejemplo anterior la equivalencia a: _SELECT ... FROM modelo WHERE campo LIKE 'OCR%'_.

```tpl
from django.contrib.auth import models

permisos_filtros = models.Permission.objects.filter(
            codename__startswith='OCR')

for permiso in permisos_filtros:
            print("Permiso filtrado: ", permiso)

```

## Filtrado normal por un campo

En este ejemplo sería un filtrado normal. Como hacer: _SELECT ... FROM modelo WHERE campo = 'OCR'_

```tpl
from django.contrib.auth import models

permisos_filtros = models.Permission.objects.filter(
            codenames='OCR')

for permiso in permisos_filtros:
    print("Permiso filtrado: ", permiso)
```

# Construyendo condiciones complejas

En Django la búsqueda con el _filter_ realiza consultas con _AND_ si queremos realizar consultas más complejas o usar el _OR_ tenemos que usar el objeto **Q** que esta en al librería:

```tpl
from django.db.models import Q
```

El objeto **Q** permite las mismas operaciones que se harían con el _filter_

## Condición a partir de un array

El siguiente ejemplo tengo una array de valores que los quiero buscar como un patron, la manera en que lo he hecho ha sido de la siguiente manera:

```tpl
# Se construye la condición a buscar en base al array con los roles
condition = Q(codename__startswith=dc.tilesRoles[0])
for role in dc.tilesRoles[1:]:
    # el |= hace un OR si se quiere un &=
    condition |= Q(codename__startswith=role)

query = models.Permission.objects.filter(condition).values()
```

La condición se inicializa con el primer registro del array. Luego se recorre el array a partir del segundo registros(los arrays en python comienzan por 0) y se van añadiendo como un _OR_, por eso se pone el _|=_.

Finalmente el objeto **Q** se pasa al _filter_ para realizar la búsqueda.

# Recuperar y procesar datos

He visto multitud de maneras recuperar y procesar la información que se obtiene al hacer un _filter_ de un modelo. Yo aquí pondré aquí lo que me ha ido bien, que no quiere decir, que sea la mejor. Simplemente me va bien para lo que yo quiero hacer.

## Recuperar datos de un _serializer_ a medida

En el siguiente ejemplo tengo un serializer a medida porque quiero recuperar los permisos globales de django cuyo nombre técnico del rol comienza por los valores de un array. Como no quiero devolver el modelo estándar de permisos lo que he hecho es un _serializer_ a medida para devolver los datos que yo quiero.

**NOTA:** Este ejemplo muy posiblmente se puede hacer en menos pasos, pero cuando lo hice era novato y prefiero dividirlo y que sea legible.

El serializer es el siguiente:

```tpl
class tilesRoles(serializers.Serializer):
    name = serializers.CharField(required=False, max_length=100)
    description = serializers.CharField(required=False, max_length=255)

    def get(self):
```

Tiene dos campos y el método _get_ es el que se llama desde la vista para recuperar los datos.

La búsqueda de datos de datos es la siguiente:

```tpl
# Lista que guardará los datos de la consulta para luego pasarlo
# al serializer
listaRoles = list()

# Se construye la condición a buscar en base al array con los roles
condition = Q(codename__startswith=dc.tilesRoles[0])
for role in dc.tilesRoles[1:]:
    # el |= hace un OR si se quiere un &=
    condition |= Q(codename__startswith=role)

    # Se lanza la query.
    query = models.Permission.objects.filter(condition).values()
```

Después del _filter_ le añado el _values()_ porque quiero recuperar los datos en formato _queryset_ este formato es el que permite ir recorriendo los datos y procesarlo como uno quiero

El siguiente paso es pasar los datos a una lista con los campos que yo quiero que luego me permitirá
instanciar la clase del _serializer_

```tpl
for role in query:
    listaRoles.append({"name": role.get("codename"),
                        "description": role.get("name")})
```

Se recorre la query guardando cada registro en un objeto que luego con el método _get_ me va a permitir recuperar del valor del campo.

Ahora se crea el "serializador" con los datos

```tpl
serializador = tilesRoles(listaRoles, many=True)
```

Se le pasa la lista y se le añade la opción _many=True_ para indicarle que van a ver muchos valores. La ventaja de pasarlo al formato _serializer_ es que luego en la vista es muy sencillo devolver los datos al formato JSON. Si pasaremos directamente la lista en la vista habría que convertirla a JSON.

La vista quedaría así de simple:

```tpl
def get(self, request, *args, **kwargs):
    # Se instancia el serializer donde se recuperan los valores
    controlador = self.serializers_class()

    # Se devuelve el resultado de la búsqueda
    return response.Response(controlador.get().data)
```
