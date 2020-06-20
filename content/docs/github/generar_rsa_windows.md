---
title: Generar clave RSA
Description: Generar clave RSA en Windows
weight: 20
---

 # Motivo

 El motivo de crear una clave RSA es poder hacer deploy a Github mediante consola y evitar el siguiente error:
 
 ![Error sin clave RSA](/images/github/generar_clave_rsa/error_sin_rsa.png)
 
 Digo consola porque es el único momento que lo he necesitado, ya que usando VS Code o la propia aplicación de Github no lo he necesitado.

 # Pasos a seguir

 1.	Abrir el git Bash. Para eso hay que tener el instalado el Git para windos y al pulsar la tecla win y al escribir git aparecerá la opción:

![Abrir Git Bash](/images/github/generar_clave_rsa/abrir_git_bash.png)

2. En el terminal que aparecerá escribir: ssh-key -t rsa -b 4096 -C "mail que nos hemos registrado en github" y al pulsar enter nos aparecerá esto:

![Generar RSA](/images/github/generar_clave_rsa/generar_rsa.png)

Pulsaremos *Enter*

Se nos pedirá introducir una frase:

![Introducir frase](/images/github/generar_clave_rsa/introducir_frase.png)

La frase se dejará en blanco para no tener que introducirla en todos los sitios que se haga push.

3.	Ahora hay que añadir la clave generada al ssh-agent. Pero para eso primero hay que valir que este en marcha. Para ello hay que introducir: 	eval $(ssh-agent –s):

![SSH Agent](/images/github/generar_clave_rsa/ssh_agent.png)

4.	Y con la siguiente instrucción se añade la clave al agente: ssh-add  ~/.ssh/id_rsa nos pedirá la frase introducida en el paso 2 y si es correcta la añadirá.

![Añadir clave](/images/github/generar_clave_rsa/anyadir_clave.png)

5.	Ahora hay que obtener la key para poderla incluirla en nuestra cuenta github, para eso hay que escribir el siguiente comando:c cat ~/.ssh/id_rsa.pub

![Clave generada](/images/github/generar_clave_rsa/clave_generada.png)

*Nota: Imagen cortada por seguridad.*
Todo lo que nos aparezca hay que copiarlo en un notepad para luego asociarlo a nuestra cuenta.

# Asociar clave a nuestra cuenta Github

Ahora hay que ir al github y acceder a los Settings de nuestro usuario e ir a la opción SSH and GPG keys:

![SSH de la cuenta de usuario](/images/github/generar_clave_rsa/ssh_cuenta_usuario.png)

Y pulsar sobre el botón *New SSH Key*:

![Introducir SSH Key](/images/github/generar_clave_rsa/anayadir_clave_rsa_cuenta.png)

El en título se puede poner lo que sea, ejemplo el ordenador donde has generado la clave, y la clave generada es la rista que hemos obtenido del comando *cat ~/.ssh/id_rsa.pub*. Una vez introducida nos aparecerá lo siguiente:

![Introducir frase](/images/github/generar_clave_rsa/resultado_anyadir_clave_usuario.png)

# Bibliografía

Para poder este artículo me he bsado en el siguiente [artículo](https://medium.com/@ancizj393/crear-una-clave-ssh-en-git-y-vincular-en-tu-cuenta-de-github-e7a6b22bc93f).