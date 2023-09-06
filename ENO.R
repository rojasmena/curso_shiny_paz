instalar = function (libreria) {
  if (libreria %in% installed.packages()[,"Package"]) {
    eval(parse(text=paste0("library(",libreria,")")))} else {
      install.packages(libreria)    
      eval(parse(text=paste0("library(",libreria,")")))
      library(libreria)
    }
}


instalar("glue")
instalar("foreign")
instalar("stringr")
instalar("readxl")
instalar("tidyverse")
instalar("dplyr")
instalar("ISOweek")
instalar("tidyr")
instalar("highcharter")
instalar("tsibble")
instalar("lubridate")
instalar("sf")
instalar("tmap")
instalar("leaflet")
instalar("DT")



# Define la lista de URLs a descargar
urls <- c(
  #1
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f4096f8b-1692-4d5f-a6d8-09cae47931a4/download/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
  #2
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/9a62f016-ba4e-4366-a883-22e9dc785d39/download/informacion-publica-respiratorias-nacional-hasta-20180626.csv",
  #3
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/0c29cc75-d81e-4f87-9257-4c30c24bfe55/download/vigilancia-de-infecciones-respiratorias-agudas-20181228.csv",
  #4
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f2998314-9087-4641-aec7-f2f67c9ba865/download/informacion-publica-respiratorias-nacional-hasta-20220905.xls",
  #5
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/37feeed4-dfcd-4f39-a403-93a6b4bd90d2/download/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx"
  # Agrega más URLs según sea necesario
)

# Define los nombres de los archivos (pueden ser diferentes a los nombres originales)
nombres_archivos <- c(
  "vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
  "informacion-publica-respiratorias-nacional-hasta-20180626.csv",
  "vigilancia-de-infecciones-respiratorias-agudas-20181228.csv",
  "informacion-publica-respiratorias-nacional-hasta-20220905.xls",
  "informacion-publica-respiratorias-nacional-hasta-20230706.xlsx"
  # Agrega más nombres de archivos según sea necesario
)

# Carpeta de destino
carpeta_destino <- "D:/Paz/Maestria/Estadistica Salud-shiny/Repositorio/curso_shiny_paz/Proyecto_ENO"


# Verifica y descarga los archivos desde las URLs
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
  } else if (extension == "xlsX") {
    datos[[i]] <- read_excel(ruta_completa)
  }
}

# Asignar a objetos con nombres específicos
for (i in 1:length(nombres_archivos)) {
  assign(paste0("datos", i), datos[[i]])
  cat("Datos", i, "cargados en objeto:", paste0("datos", i), "\n")
}


#como no funciono lo anterior
datos1 <-read.csv("Proyecto_ENO/informacion-publica-respiratorias-nacional-hasta-20180626.csv",
         encoding = "UTF-8")
datos2 <-read.csv("Proyecto_ENO/vigilancia-de-infecciones-respiratorias-agudas-20181228.csv",
                  encoding = "UTF-8")
datos3 <-read.csv("Proyecto_ENO/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
                  encoding = "UTF-8")
datos4 <- read_excel("Proyecto_ENO/informacion-publica-respiratorias-nacional-hasta-20220905.xls")
datos5 <- read_excel("Proyecto_ENO/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx")


# Lista de nombres de los dataframes
nombres_dataframes <- c("datos1", "datos2", "datos3", "datos4", "datos5")

# Loop para mostrar los nombres de las columnas de cada dataframe
for (nombre_df in nombres_dataframes) {
  if (exists(nombre_df)) {
    cat("Nombres de columnas en", nombre_df, ":\n")
    print(colnames(get(nombre_df)))
    cat("\n")
  } else {
    cat("El dataframe", nombre_df, "no existe.\n")
  }
}


##identifico que algunos dataframes tienen error en el nombre de la col 5 (año)

#la renombro
# Loop para reemplazar el nombre de la columna 5  por "ano" en cada dataframe
for (nombre_df in nombres_dataframes) {
  if (exists(nombre_df)) {
    df <- get(nombre_df)  # Obtener el dataframe
    if (ncol(df) == 10) {
      colnames(df)[5] <- "ano"  # Reemplazar el nombre de la columna 5 por "ano"
      assign(nombre_df, df)  # Actualizar el dataframe en el entorno
      cat("Nombre de la columna 5 reemplazado por 'ano' en", nombre_df, "\n")
    } else {
      cat("El dataframe", nombre_df, "tiene un numero diferente de columnas, probablemente tenga otra estructura\n")
    }
  } else {
    cat("El dataframe", nombre_df, "no existe.\n")
  }
}
