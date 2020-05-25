# Proyecto Final Análisis Númerico 2020-01

## Pontificia Universidad Javeriana || Ingeniería de sistemas

## Integrantes:
- Juan José Camacho [TheCodingJuan](https://github.com/TheCodingJuan).
- Gabriela Maria Camacho [gabriela10ca](https://github.com/gabriela10ca).

## Instalación
1. Entorno de desarrollo
- El aplicativo fue desarrollado en el lenguaje [R](https://www.r-project.org/other-docs.html).
- IDE usado para el desarrollo del mismo [RStudio](https://docs.rstudio.com/).

2. Librerias que fueron usadas para el desarrollo del aplicativo
- [Shinny Dashboard](https://shiny.rstudio.com/articles/dashboards.html) Usada para la creación de GUI.
- [Shinny](https://www.rdocumentation.org/packages/shiny/versions/1.4.0.2) Usada para creación de GUI.
- [ODE](https://www.rdocumentation.org/packages/deSolve/versions/1.27.1/topics/ode) Usada para el cálculo de Ecuaciones Diferenciales.
- [lSoda](https://www.rdocumentation.org/packages/deSolve/versions/1.27.1/topics/lsoda) Método alternativo para el cálculo de Ecuaciones Diferenciales.
- [lSode](https://www.rdocumentation.org/packages/deSolve/versions/1.27.1/topics/lsode) Método alternativo para el cálculo de Ecuaciones Diferenciales.
- [Pracma](https://www.rdocumentation.org/packages/pracma/versions/1.9.9). Provee funciones para al análisis númerico.
3. Uso de las librerias
Poner este codigo al inicio de su archivo R.

 ```r
library(shiny)
library(shinydashboard)
library(deSolve)
library(phaseR)
library(pracma)
library(dplyr)
library(ggplot2)
library(ggthemes)
 ```
 

## Datos 
-  [Datos SARS-CoV-2](https://github.com/datadista/datasets/tree/master/COVID%2019) Fuente principal de datos. En este repositorio se encuentran las entradas en España sobre el virus Datos SARS-CoV-2.

## Referencias
- [Coronavirus: El factor R y los cuatro parámetros de contagio](https://www.dw.com/es/coronavirus-el-factor-r-y-los-cuatro-par%C3%A1metros-de-contagio/a-53212796).
- [Curva del coronavirus: tasa de reproducción del Covid-19 en España](https://www.redaccionmedica.com/secciones/interactivos/curva-coronavirus-evolucion-3104).

## Modelos implementados
 
 - [SIR](http://www.mat.ucm.es/~ivorra/papers/Diego-Epidemiologia.pdf).
 - [SIS](https://biblioteca.unirioja.es/tfe_e/TFE002211.pdf).
 
## Variables esenciales para el cálculo
- Poblacion.
- Indice de contactos promedio del ser humano.
- Probablidad de contagio.
- Cantidad inicial de infectados.
- Gamma (Tasa de contagio en una población).
- Beta (Tasa de recuperacion en una poblacion).
