library(shiny)
library(readxl)
library(tidyverse)
library(highcharter)
library(bslib)
library(shinyjs)


#descargo el dataset 
# Define la lista de URLs a descargar
urls <- c(
  #Casos 1-1-22 al 2-7-23 (SE26) (xlsx)
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/37feeed4-dfcd-4f39-a403-93a6b4bd90d2/download/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx",
  #Casos SE01/20 a SE32/22 (xlsx)
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f2998314-9087-4641-aec7-f2f67c9ba865/download/informacion-publica-respiratorias-nacional-hasta-20220905.xls",
  #Casos SE18/2018 a SE52/2019 (CSV POR COMAS)
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f4096f8b-1692-4d5f-a6d8-09cae47931a4/download/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv")


# Define los nombres de los archivos
nombres_archivos <- c(
  "Respi-SE1-22-a-SE26-23.xlsx",
  "Respi-SE1-20-a-SE32-22.xls",
  "Respi-SE18-18-a-SE52-19.csv")

carpeta_destino <- getwd()

#Verifica y descarga los archivos desde las URLs
for (i in 1:length(urls)) {
  # Ruta completa del archivo en la carpeta /datos
  ruta_completa <- file.path(carpeta_destino, nombres_archivos[i])
  
  # Verifica si el archivo ya existe en la carpeta
  if (!file.exists(ruta_completa)) {
    # Si el archivo no existe, descárgalo desde la URL
    download.file(urls[i], destfile = ruta_completa, mode = "wb")
    cat("Archivo descargado y guardado en:", ruta_completa, "\n")
  } else {
    cat("El archivo ya existe en la carpeta:", ruta_completa, "\n")
  }
}


# Leer los archivos y asignar a objetos en R
datos <- list()  # Lista para almacenar los datos

for (i in 1:length(nombres_archivos)) {
  ruta_completa <- file.path(carpeta_destino, nombres_archivos[i])
  extension <- tools::file_ext(nombres_archivos[i])
  
  if (extension == "csv") {
    datos[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xls" || extension == "xlsx") {
    datos[[i]] <- read_excel(ruta_completa)
  }
}

# Asignar a objetos con nombres específicos
for (i in 1:length(nombres_archivos)) {
  assign(paste0("datos", i), datos[[i]])
  cat("Datos", i, "cargados en objeto:", paste0("datos", i), "\n")
}

#datos 1 se conserva completa
#datos 2 conservo 2020 y 2021
#datos 3 conservo 2019

#union de dataset
datos1=datos1 %>% rename(anio=año)
datos2=datos2 %>% rename(anio=año)

agrupado_clinica=rbind(datos1, filter(datos2,anio==2020|anio==2021), filter (datos3,anio==2019))



#corregir nombres de provincias

agrupado_clinica <- agrupado_clinica %>% 
  mutate(
    provincia_nombre= case_when(
      .$provincia_id == 6 ~ "Buenos Aires",
      .$provincia_id == 2 ~ "Ciudad Autónoma de Buenos Aires",
      .$provincia_id == 30~ "Entre Ríos",
      .$provincia_id == 82~ "Santa Fe",
      .$provincia_id == 14~ "Córdoba",
      .$provincia_id == 38~ "Jujuy",
      .$provincia_id == 66~ "Salta",
      .$provincia_id == 90~ "Tucumán",
      .$provincia_id == 86~ "Santiago del Estero",
      .$provincia_id == 46~ "La Rioja",
      .$provincia_id == 10~ "Catamarca",
      .$provincia_id == 70~ "San Juan",
      .$provincia_id == 74~ "San Luis",
      .$provincia_id == 50~ "Mendoza",
      .$provincia_id == 42~ "La Pampa",
      .$provincia_id == 62~ "Río Negro",
      .$provincia_id == 58~ "Neuquén",
      .$provincia_id == 26~ "Chubut",
      .$provincia_id == 78~ "Santa Cruz",
      .$provincia_id == 94~ "Tierra del Fuego",
      .$provincia_id == 18~ "Corrientes",
      .$provincia_id == 54~ "Misiones",
      .$provincia_id == 22~ "Chaco",
      .$provincia_id == 34~ "Formosa"
    ))


#creo variable region

agrupado_clinica <- agrupado_clinica %>% 
  mutate(
    region= case_when(
      .$provincia_id == 6 ~ "Centro",
      .$provincia_id == 2 ~ "Centro",
      .$provincia_id == 30~ "Centro",
      .$provincia_id == 82~ "Centro",
      .$provincia_id == 14~ "Centro",
      .$provincia_id == 38~ "NOA",
      .$provincia_id == 66~ "NOA",
      .$provincia_id == 90~ "NOA",
      .$provincia_id == 86~ "NOA",
      .$provincia_id == 46~ "NOA",
      .$provincia_id == 10~ "NOA",
      .$provincia_id == 70~ "Cuyo",
      .$provincia_id == 74~ "Cuyo",
      .$provincia_id == 50~ "Cuyo",
      .$provincia_id == 42~ "Sur",
      .$provincia_id == 62~ "Sur",
      .$provincia_id == 58~ "Sur",
      .$provincia_id == 26~ "Sur",
      .$provincia_id == 78~ "Sur",
      .$provincia_id == 94~ "Sur",
      .$provincia_id == 18~ "NEA",
      .$provincia_id == 54~ "NEA",
      .$provincia_id == 22~ "NEA",
      .$provincia_id == 34~ "NEA"
    ))



#CLASIFICO ETI/BQL/NMN

agrupado_clinica <- agrupado_clinica %>% 
  mutate(
    evento= case_when(
      evento_nombre == "Bronquiolitis en menores de 2 anos"|
        evento_nombre == "Bronquiolitis en menores de 2 años (sin especificar)"|
        evento_nombre == "Bronquiolitis en menores de 2 años ambulatorios"|
        evento_nombre == "Bronquiolitis en menores de 2 años internados" ~ "Bronquiolitis en menores de 2 años",
      evento_nombre == "Enfermedad tipo influenza (ETI)"~ "Enfermedad tipo influenza (ETI)",
      evento_nombre == "Neumonia"|
        evento_nombre=="Neumonía (sin especificar)"|
        evento_nombre=="Neumonía en pacientes ambulatorios"|
        evento_nombre=="Neumonía en pacientes internados"~ "Neumonía",
      TRUE~"Otro"))


#poblacion para incidencia

pobl=read.csv2("proyecciones_edad_provincia_base.csv", encoding="latin1") %>% 
  filter(ano %in% c(2019:2023) & sexo_codigo==0) %>% 
  mutate(
    juri_nombre= case_when(
      .$juri == 6 ~ "Buenos Aires",
      .$juri == 2 ~ "Ciudad Autónoma de Buenos Aires",
      .$juri == 30~ "Entre Ríos",
      .$juri == 82~ "Santa Fe",
      .$juri == 14~ "Córdoba",
      .$juri == 38~ "Jujuy",
      .$juri == 66~ "Salta",
      .$juri == 90~ "Tucumán",
      .$juri == 86~ "Santiago del Estero",
      .$juri == 46~ "La Rioja",
      .$juri == 10~ "Catamarca",
      .$juri == 70~ "San Juan",
      .$juri == 74~ "San Luis",
      .$juri == 50~ "Mendoza",
      .$juri == 42~ "La Pampa",
      .$juri == 62~ "Río Negro",
      .$juri == 58~ "Neuquén",
      .$juri == 26~ "Chubut",
      .$juri == 78~ "Santa Cruz",
      .$juri == 94~ "Tierra del Fuego",
      .$juri == 18~ "Corrientes",
      .$juri == 54~ "Misiones",
      .$juri == 22~ "Chaco",
      .$juri == 34~ "Formosa",
      .$juri == 1~ "TOTAL DEL PAÍS"
    )) %>% 
  mutate(
    region= case_when(
      .$juri == 6 ~ "Centro",
      .$juri == 2 ~ "Centro",
      .$juri == 30~ "Centro",
      .$juri == 82~ "Centro",
      .$juri == 14~ "Centro",
      .$juri == 38~ "NOA",
      .$juri == 66~ "NOA",
      .$juri == 90~ "NOA",
      .$juri == 86~ "NOA",
      .$juri == 46~ "NOA",
      .$juri == 10~ "NOA",
      .$juri == 70~ "Cuyo",
      .$juri == 74~ "Cuyo",
      .$juri == 50~ "Cuyo",
      .$juri == 42~ "Sur",
      .$juri == 62~ "Sur",
      .$juri == 58~ "Sur",
      .$juri == 26~ "Sur",
      .$juri == 78~ "Sur",
      .$juri == 94~ "Sur",
      .$juri == 18~ "NEA",
      .$juri == 54~ "NEA",
      .$juri == 22~ "NEA",
      .$juri == 34~ "NEA"
    )) %>% 
  group_by(ano,region,juri_nombre) %>% 
  summarise(pob=sum(poblacion, na.rm=T))

resultado = list(
  agrupado_clinica = agrupado_clinica,
  datos = datos,
  datos1 = datos1,
  datos2 = datos2,
  datos3 = datos3,
  pobl = pobl
)

save(resultado, file = "resultado.RData")
