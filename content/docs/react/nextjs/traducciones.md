---
title: Traducciones
description: Traducciones
---

# Introducción

Por defecto ni React ni NextJS permite hacer textos multiidioma. Pero al ser un sistema abierto alguien se ha trabajado una serie de plug-in para conseguirlo. El objetivo es explicar como hacerlo.

Los pasos vamos a dividirlo en dos:

1. Será la instalación, configuración y funcionamiento del *Next-i18n* para poder los textos multiidioma.
2. Detectar el idioma para pasarselo al Next-i18n.

# Instalación y configuración del *Next-i18n*

## Instalación

Hay que instalar el paquete [next-i18next](https://github.com/isaachinman/next-i18next):

```tpl
npm install next-i18next
```

## Configuración

**Nota Aclaratoria:** Pongo los datos en que me ha funcionado, ya que con el ejemplo que ponen en el paquete no funciona. 

Hay que crear, si no lo esta, el fichero *next.config.js* en la carpeta ráiz del proyecto y añadir lo siguiente:

```tpl
const { nextI18NextRewrites } = require("next-i18next/rewrites");

const localeSubpaths = {};

module.exports = {
  rewrites: async () => nextI18NextRewrites(localeSubpaths),
  publicRuntimeConfig: {
    localeSubpaths,
  },
};

Esto lo que hace es permitir los path por idioma a nivel de URL. 

En la carpeta raíz del proyecto hay que crear el archivo *i18n.js* y añadir lo siguiente:
```tpl
const NextI18Next = require("next-i18next").default;
const { localeSubpaths } = require("next/config").default().publicRuntimeConfig;
const path = require("path");

module.exports = new NextI18Next({
  defaultLanguage: "es",
  fallbackLng: "es",
  otherLanguages: ["en"],
  localeSubpaths,
  localePath: path.resolve("./public/static/locales"),
});
```

Esto lo que hace es instanciar la clase *NextI18Next* que es la que se usará para las traducciones. Cosas a tener en cuenta:

* El parámetro *defaultLanguage* no me funciona. Me toma el idioma *en*. Pero con el parámetro *fallbackLng* soluciono el problema porque la carpeta *en* de los locales no esta creada, por lo tanto busca el idioma de dicho parámetro.
* en el parámetro *localPath* se indica en que directorio hay que añadir los archivos de traducción. Luego se verá su formato.

En la carpeta *pages* hay que modificar el archivo *_app.js*(en la últimas versiones de *NextJS* ya viene por defecto) y añadir lo siguiente:
```tpl
import App from "next/app";
import { appWithTranslation } from "i18n";


const OCRApp = ({ Component, pageProps }) => {
  return (
      <Component {...pageProps} />;    
  );
};

OCRApp.getInitialProps = async (appContext) => {
  const appProps = await App.getInitialProps(appContext);
  return { ...appProps };
};

export default appWithTranslation(OCRApp);
```

Este archivo es un poco distinto a lo que explican en el ejemplo del paquete, porque a declarar la aplicación *OCRApp* faltaba el *return*. 

Aunque NextJS recomienda no usar el *getInitialProps*, hay que usarlo por necesidad ya que el paquete no funciona bien con los *getServerSideProps* o *getStaticProps*, no falla, pero da un warning en consola constantemente sobre que falta declarar el *namespacesRequired*. Cosa que no ocurre si se usa el *getInitialProps*.

## Funcionamiento

### Archivos *locales*

Dentro dela carpeta *public* del proyecto hay que crear la carpeta *locales*, dentro de ella el código del idioma y dentro de la carpeta del idioma el fichero *common.json*. Este fichero es que el que busca por defecto. La estructura en mi caso queda así:

```
public
|--- locales
|    |--- es
|    |    |--- common.js
```

Ejemplo del fichero es el siguiente:

```tpl
{
    "AppTitle": "OCR de Facturas",
    "PageLoginTitle": "Datos de acceso",
    "loginLblUsername": "Usuario",
    "loginLblPassword": "Password"
}
```

El nombre del las etiquetas no puede tener ni "." ni "_", ni supongo que otros carácteres, ya que sino no funciona. Probado personalmente porque en *Open UI5* suelo usar el "." para separar cada parte de la etiqueta.

**NOTA IMPORTANTE:** Se pueden crear ficheros con otros nombres, pero el *common* es el que ira a buscar por defecto. Será en las páginas/componentes donde se indique el fichero que se quiere leer.
**NOTA IMPORTANTE 2:** Los cambios que se hagan en los ficheros de traducción implica tener que reiniciar el servidor para tomar los cambios.

### Usando en páginas y componentes

Un ejemplo de como usarlo en una página o componentes es el siguiente:

```tpl
import PropTypes from "prop-types";
import { i18n, Link, withTranslation } from "i18n";

const login = ({ t }) => {
  return (
    <div className="container">
      <div className="jumbotron">
        <p className="lead text-center">{t("PageLoginTitle")}</p>
       
      </div>
    </div>
  );
};

login.getInitialProps = async () => {
  return { namespacesRequired: ["common"] };
};

login.propTypes = {
  t: PropTypes.func.isRequired,
};

export default withTranslation("common")(login);

```
Cosas a tener en cuenta:

* El *{t}* que son los parámetros de la vista siempre será *t* ya que es el que se pasa al hacer *withTranslation("common")(login)*
* El texto se recupera usando *{t("<nombre de la etiqueta>")}* 
* El *namespacesRequired* se pone porque lo indica la [docu oficial](https://github.com/isaachinman/next-i18next#4-declaring-namespace-dependencies) para temas de rendimiento. Ya que sino se indica se lee todos los ficheros posibles, penalizando el rendimiento. Aun poniendolo me da el warning en la consola del servidor *NextJS*.

Cosas que que no tengo claras:

* El *.propTypes* es una validación para que el parámetro *t* sea obligatorio. Pero tengo claro que sea necesarioa. Ya que poniendolo, como si no, funciona todo bien. Y parámetro o props *t* se tienen que poner si o si.


# Detectar el idioma

Hay que instalar el paquete: [i18next-browser-languageDetector](https://github.com/i18next/i18next-browser-languageDetector) mediante el siguiente comando:
```tpl
npm install i18next-browser-languagedetector
```

**NOTA:NO SOY CAPAZ DE HACERLO FUNCIONAR DIRECTAMENTE CON NEXT-I18NEXT** Encontrará soluciones alternativas.




