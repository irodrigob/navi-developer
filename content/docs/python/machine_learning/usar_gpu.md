---
title: Uso GPU
description: Usar GPU para utilizar el machine learning
---

# Introducción

El objetivo es explicar que he hecho para poder hacer utilizar la GPUs de la tarjeta gráfica para las operaciones del machine learning. El motivo, es que una tarjeta gráfica tiene muchos más cpus que se pueden usar paralelamente para los calculos.

Se va explicar como se ha usado para TensorFlow pero puede servir para otras librerías. 

**NOTA IMPORTANTE: El sistema operativo es Windows**

# Tensor Flow

Lo primero que hay que hacer es mirar la [documentación oficial](https://www.tensorflow.org/install/gpu) para averiguar que versión de librerías es necesaria.

En el momento que se hace esta página Tensor Flow es compatible con CUDA 10.1. Por lo tanto hay que seguir los siguientes pasos:

## Instalar CUDA

Descargar e instalar la versión de [CUDA](https://developer.nvidia.com/cuda-toolkit-archive) que sea compatible con TensorFlow. En la instalación solo he indicado que instale las librerías *runtime*. La integración con Visual Studio, ejemplos, etc.. no lo he seleccionado.

Una vez instalado hay que añadir el path donde se ha instalado a la variable de entorno *PATH* del sistema. Aunque cuando se entre al ver las variables de entorno aparece el *CUDA_PATH* pero no funciona. Hay que añadir la misma ruta que esta en esa variable en la *PATH*. Y luego reiniciar el sistema.

## Instalar CUDnn

Es la librería que usa TensorFlow para poder usar el CUDA de la tarjeta gráfica. En esta [página](https://developer.nvidia.com/rdp/cudnn-archive) hay que descargar la versión asociada a la versión de CUDA. En la página ya lo indica.  Hay que tener en cuenta que hay que registrarse para podersela descargar.

Se descarga un fichero que hay que descomprimirlo donde uno quiera, pero luego hay que añadir la *ruta+\bin* en el variable de entorno *PATH*. Ejemplo yo lo tengo instalado en *K:\cuda*. Y la variable de entorno he puesto *K:\cuda\bin*. Un vez hecho hay que reiniciar el pc.

## Creación entorno en Anaconda

En anaconda he creado un entorno nuevo llamado *tensorflow_gpu* donde la versión escogida de Python es la 3.8. Una vez creado he ido al *Anaconda PowerShell* para poder instalar los paquetes a través de *PiP* que es el instalador de paquetes de Python. El motivo de hacerlo de esta manera es que a través de anaconda he tenido muchos conflictos con las versiones de Python y librerias. Y mirando por internet he visto que esta solución me funciona ya que me instala paquetes más recientes que los que haría con Anaconda.

Dentro del powershel de Anaconda he hecho lo siguiente:

```tpl
conda activate tensorflow-gpu
pip install tensorflow
pip install tensorflow-gpu
```
Estas dos librerías instalarán todas las librerías que necesiten.

Aunque se instale con *PiP* en Anaconda veremos lo que se ha instalado:

![Tensor Flow](/images/python/machine_learning/anaconda/paquetes_usando_pip.png)

Para desintalar habría que hacer:
```tpl
pip uninstall tensorflow
pip uninstall tensorflow-gpu
pip uninstall protobuf
```

## Solución al error *Could not create cudnn handle: CUDNN_STATUS_INTERNAL_ERROR*

Este error aparece por un problema de memoria de las CPUs de NVIDIA. Para solucionar hay que poner las siguiente líneas después del *import* de la librería de TensorFlow y antes del código principal:

```tpl
physical_devices = tf.config.experimental.list_physical_devices('GPU')
assert len(physical_devices) > 0, "Not enough GPU hardware devices available"
config = tf.config.experimental.set_memory_growth(physical_devices[0], True)
```

