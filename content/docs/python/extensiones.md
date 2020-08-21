---
title: Extensiones
description: Extensiones VS Code
weight: 20
---

# Introducción

Extensiones, sobretodo para VS Code, que son útiles para desarrollar

# Python-autopep8

## ¿Qué és?

Extensión que formatea en bonito y siguiendo *best-practices* el código que se desarrolla.

## Instalación

Se instala directamente desde las extensiones del visual code

# Pylint

## ¿Qué és?

[Librería](https://www.pylint.org/) que verifica sintacticamente que el código sea correcto.

## Instalación

Se hace con *Anaconda Power Shell* en el entorno adecuado con lo siguiente:

```tpl
conda activate <entorno>
pip install pylint
```

# Pylint

## ¿Qué és?

[Librería](https://pypi.org/project/pylint-django/) que verifica sintacticamente que el código sea correcto cuando se usa la librería *Django*

## Instalación

Se hace con *Anaconda Power Shell* en el entorno adecuado con lo siguiente:

```tpl
conda activate <entorno>
pip install pylpylint-django
```

Luego hay que ir a VSCode y pulsar *CTRL+SHIFT+P* para abrir el *command palette* y escribir *Preferences: Configure Language Specific Settings*. Saldrá un desplegable con el lenguaje a seleccionar, se escoge *Python*. Y en la configuración añadir el siguiente código:

```tpl
"python.linting.pylintArgs": [
        "--load-plugins=pylint_django",
    ]
```