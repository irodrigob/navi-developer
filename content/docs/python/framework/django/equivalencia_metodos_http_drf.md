---
title: Equivalencia métodos HTTP y DRF
description: Equivalencia métodos HTTP y Djando Rest Framework
---

# Equivalencia en vistas de tipo *ViewSet*

En las vistas de tipo *ViewSet* si queremos sobrecargar algun tipo de llamada HTTP hay que saber que método sobrecargar. La equivalencia es la siguiente:

| HTTP   | Método ViewSet                                                                                      |
|--------|-----------------------------------------------------------------------------------------------------|
| GET    | Si queremos una lista de datos el método es *list*. Si es un valor concreto el método es *retrieve* |
| POST   | create                                                                                              |
| PUT    | update                                                                                              |
| PATCH  | partial_update                                                                                      |
| DELETE | destroy                                                                                             |