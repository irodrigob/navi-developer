---
title: Publicar Web Github
Description: Como publicar una web en Github
weight: 20
bookCollapseSection: true
---

# Motivo

Para aquellos que no quieren gastarse dinero en productos como Wordpress o no necesitan páginas dinámicas, la opción de crear una de manera gratuíta y usando Hugo es una buena elección. 

Personalmente la clave es escoger un tema que se nos adapte a lo que necesitamos, porque cada tema tiene ciertos comportamientos que no tiene otros.  Por ejemplo, inicialmente había escogido el tema *npp-hugo*, cuando empece a montar la estructura que quería vi que no era tan flexible en la organización por carpetas como me esperaba (al menos es lo que he visto probandolo). Por ello finalmente y después de revisar unos cuantos temas he optado por el tema [book](https://themes.gohugo.io/hugo-book/) que se ajustaba más lo que buscaba. 

Por ello creo que para usar Hugo es básico escoger con calma el tema que se quiera ya que funcionalidad que tiene un tema no existen en otro. Por ejmeplo, hay páginas template, *mardownsyntax.md* del tema *npp-hugo* no son compatibles con el tema *book* que uso. En mi caso me he dado cuenta mientras hacía pruebas con este tema. 

Aparte he visto que la documentación oficial parece echa para construir template porque realmente lo que me ha servido para construir la página han sido los ejemplos que vienen incluídos en la carpeta *exampleSite* que suele vernir en cada tema.

# Pasos

## Pasos previo

Antes de iniciar el proceso se aconseja instalar la clave RSA en local de Github siguiendo los pasos de este [artículo](/docs/github/generar_rsa_windows.md)

## Instalar Hugo

 Hugo se puede descargar de la siguiente página [web](https://gohugo.io/getting-started/installing/). Una vez instalado es aconsejable poner el directorio donde se ha instalado en el *PATH* de window para un mejor acceso:

 ![path Hugo](/images/hugo/publicar_web/path_hugo.png)

## Crear los repositorios en el Github

 El sitio que se va a crear se tiene que sincronizar con dos repositorios que se han de crear previamente. Estos sitios son:

 * *<nombre del sitio>*  será el encargado de tener los ficheros del sitio que estamos construyendo. El que contendrá los ficheros *.md*
 * *<nombre del usuario>.github.io* será el encargado guardar las páginas renderizadas por Hugo en base al contenido en el sitio anterior. **Es importante poner el nombre del usuario ya que si no, no funcionara**. En mi caso el repositorio creado es: *irodrigob.github.io*


## Crer el sitio en local

Hay que abrir la consola de Windows e ir al directorio donde vamos crear el sitio web. Una vez en el directorio hay que escribir: *hugo new site nombre_sitio*:

![Resultado crear sitio](/images/hugo/publicar_web/resultado_crear_sitio.png)

## Añadir el tema

En la página web de los [temas](https://themes.gohugo.io/) se pueden encontrar multitud de temas para descargar. Para este sitio se ha utilizado el tema [book](https://themes.gohugo.io/hugo-book/) 

* NOTA: Tanto los pantallazos como en las explicación sale el tema npp-hugo, esto es debido a que use incialmente dicho tema para crear el sitio.*

Para instalarlo hay que hacerlo desde la consola hay que hacer los siguientes pasos:

* ```git init```, Crea los archivos para poder sincronizar el sitio con Git
* ```git submodule add https://github.com/saadnpq/npq-hugo themes/npqq-hugo```, Añade como submodulo el tema y lo descargará  en el directorio *themes/npqq-hugo*
* Editar el fichero *config.toml* que esta en el directorio raíz del sitio y poner el contenido del mismo fichero, que hay en el directorio *themes/npqq-hugo/example-config.toml*. Una vez copiado se ajustarán los datos de configuración básicos.

En consola se irá viendo lo siguiente:

![Añadir tema](/images/hugo/publicar_web/anaydir_tema.png)

Con esto ya se tiene un primer esbozo del sitio, para ver cómo queda en la consola hay que escribir: ```hugo server -D```

Si abrimos una navegador e introducimos la url: ```http://localhost:1313/``` veremos como queda el sitio.

## Sincronizar el sitio local con Gihub

Esta es la parte que mas me ha costado por no estar acostumbrado a trabajar con Git. Esta es la parte es la que he aprendido que hacer que hacerlo en consola, ya que aunque puedes hacer los push con la aplicación de escritorio de Github o con el VS Code(que es el que uso para crear las páginas) he descubierto, seguramente por desconocimiento, que no funciona igual y se jode de tal manera los archivos de Git que he tenido que borrar sitio y repositorios más de una vez. Ahora, todo lo hago con la consola y ya no tengo errores.

Personalmente creo que si los submodulos (solo hemos creado el primero de ellos, el del tema) se hubiesen sincronizado con el Github Desktop no hubiese habido problemas. Pero ahora mismo no tengo ganas de más experimentos.

Los dos sitios del github los hemos creado en el paso anterior, aunque también es posible crearlo justo en este momento, pero así es como lo he hecho yo. Una vez creados hay que seguir los siguientes pasos para sincronizalos en local.

Desde la consola de Windows y estando en el directorio del sitio hay que ejecutar los siguientes comandos git:
* ```git remote add origin git@github.com:irodrigob/navi-developer.git``` Añade el repositorio que guardará las páginas que vamos creando
* ```git add .``` Le decimos que queremos añdir todas las carpetas del sitio en el git
* ```git commit -m "Commit inicial del sitio"``` Guarda los ficheros añadidos en el paso anterior en el git local.

A nivel de consolo iremos viendo algo parecido a esto:

![Resultado Git repositorio](/images/hugo/publicar_web/resultado_git_repositorio_hugo.png)


Ahora hay que añadir el repositorio donde se guardarán las páginas renderizadas por Hugo:

* ```git submodule add git@github.com:irodrigob/irodrigob.github.io.git``` En este sentencia aparecerá la pregunta de la frase de la clave RSA que hemos hecho previamente para poder subir a Github:

![Frase RSA submódulo](/images/hugo/publicar_web/frase_rsa_submodulo.png)

Aunque en la imagen no se vé, hay una pregunta que pide poner: *Yes/No/Phrase*. Como no se ha puesto frase, para ser más comodo las subidas, hay que poner *Yes* para confirmar la clave y así no volverá a realizar la misma pregunta.

Una vez añadido hay que hacer los siguientes pasos:

* ```git add .```
* ```git commit -m "Commit inicial"```
* ```git push –u origin master``` Este sube los archivos del repositorio local al remoto

A nivel de consola deberá aparecer algo parecido a esto:

![Resultado Git submodulo](/images/hugo/publicar_web/resultado_git_submodulo.png)

Hasta ahora solo hemos subido datos del repositorio que hemos creado para el sitio. Ahora hay que cambiar la configuración para que Hugo publique las páginas en nuestro repositorio de *github.io*

## Publicando en *github.io*

El archivo de configuración *config.toml* hay que realizar los siguientes cambios:
* Al parámetro *baseURL* se le tiene que poner nuestra dirección del repositorio creado con el nombre *github.io*
* Se añade el parámetro *publisDir* con el nombre de la carpeta que se ha creado al crear el submodulo del repositorio *github.io". Este directorio es donde Hugo rendizara las páginas *markdow* a *HTML*

Así es como quedaría(*Nota: Ejemplo del fichero que estoy usando con tema que estoy usando actualmente*):

```
baseURL = "https://irodrigob.github.io/"
languageCode = "es-es"
title = "Navi-developer"
theme = "hugo-book"
publishDir = "irodrigob.github.io" 
```

Ahora de nuevo vía consola, y en el directorio del sitio, con el comando ```hugo``` se creará la página Web en el directorio del submodulo *github.io*

Ahora y antes de hacer el pull para subir los archivos al repositorio *github.io" vamos a comprobar que la configuración sea la correcta. Para ello de nuevo desde la consola se escribirá: ```git remote -v```:

![Resultado git remote](/images/hugo/publicar_web/resultado_git_remote.png)

Si el resultado es como el de la imagen es que todo es correcto.

Ahora para poder subir los archivos generados a nuestro repositorio *github.io* hay que ir al directorio de dicho repositorio y lanzar los siguientes comandos en la consola de windows:

```
git add .
git commit -m "Primera subida!!"
git push origin master
```

En consola aparecerá algo parecido a esto:

![Resultado pull github.io](/images/hugo/publicar_web/resultado_pull_githubio.png)


Y ahora si se accede la siguiente página: ```http://irodrigob.github.io/``` se verá el contenido publicado.

# Bibliografía

Para poder este artículo me he bsado en el siguiente [artículo](https://inside.getambassador.com/creating-and-deploying-your-first-hugo-site-to-github-pages-1e1f496cf88d).