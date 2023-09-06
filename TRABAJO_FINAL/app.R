#Paquetes

library("dplyr")
library("shiny")
library("tidyverse")
library("tidyr")
library("DT")
library("lubridate")
library("shinyWidgets")
library("highcharter")

#descargo el dataset 

options(timeout=1000000) # incrementamos el timeout debido a que la descarga es lenta

url = "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv"

download.file(url, destfile = "TMI.csv")

data = read.csv("TMI.csv")
unlink("TMI.csv")

DT::datatable(data)