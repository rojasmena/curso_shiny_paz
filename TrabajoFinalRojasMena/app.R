library(shiny)
library(readxl)
library(tidyverse)
library(highcharter)
library(bslib)
library(shinyjs)

#Tema
TemaTrabajo=bs_theme(bootswatch = "lumen")
TemaTrabajo=bs_theme_update(TemaTrabajo, primary = "#238FB8", success = "#A4E9FF", 
                             warning = "#F5F5C4", font_scale = NULL, preset = "lumen")

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

carpeta_destino <- "D:/Paz/Maestria/Estadistica Salud-shiny/Repositorio/curso_shiny_paz"

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
  evento_nombre == "Bronquiolitis en menores de 2 años internados" ~ "Bronquiolitis",
  evento_nombre == "Enfermedad tipo influenza (ETI)"~ "Enfermedad tipo influenza (ETI)",
  evento_nombre == "Neumonia"|
  evento_nombre=="Neumonía (sin especificar)"|
  evento_nombre=="Neumonía en pacientes ambulatorios"|
  evento_nombre=="Neumonía en pacientes internados"~ "Neumonía",
  TRUE~"Otro"))


#  
  ui <- fluidPage(
      # titulo
      useShinyjs(),
      theme=TemaTrabajo, 
      fluidRow(
        column(width = 12,
               h1(strong("Enfermedades Respiratorias Agudas")),
               align = "center"
        )
      ),
      
      hr(),
      
      
      # primera fila de la ui
      fluidRow(
        column(width = 3,
                 selectInput(
                 inputId = "SelectEvento",
                 label = "Seleccionar Evento Clínico:",
                 choices = unique(agrupado_clinica$evento),
                 selected = "Enfermedad tipo influenza (ETI)"
               )),
        column(width = 3,
               selectInput(
                 inputId = "SelectAreaGeografica",
                 label = "Seleccionar Área Geográfica:",
                 choices = c("Total Nacional", "Región","Jurisdicción") ,
                 selected = "Total Nacional"
               ),
               uiOutput("AreaGeograficaInputs")),
               
        column(width = 3,
               selectInput(
                 inputId = "TipoGrafico",
                 label = "Gráfico:",
                 choices = c("Columnas", "Curva Epidémica") ,
                 selected = "Columnas"
               ),
               checkboxInput(
                 "Incidencia",
                 "Mostrar tasas de incidencia"
               )),
        column(width = 3,
               checkboxGroupInput(
                 inputId = "SelectAnio",
                 label = "Seleccionar Años:",
                 choices = unique(agrupado_clinica$anio),
                 selected = c("2019", "2020", "2021", "2022", "2023"
                              )
               ),
               selectInput(
                 inputId = "SelectSEPI",
                 label = "Hasta semana epidemiológica:",
                 choices = seq(1,53) ,
                 selected = "53"
                 ),
               actionButton(
                 "procesar",
                 "Procesar"
               ),
               disabled(actionButton(
                 "limpiar",
                 "Limpiar filtro"
               )),
               )
        ),
      
      fluidRow(
        column(
          width = 6,   
          align = "center",
          h2("Gráfico"),
          hr(),
          highchartOutput("graficoColumnas") #corregir ubicacion
        )
      )
)

      



server <- function(input, output, session) {
 
  session$onSessionEnded(function() {
    stopApp()
  })
  
 #activación de botones 
  
  output$AreaGeograficaInputs <- renderUI({
    area_geografica <- input$SelectAreaGeografica
    
    if (area_geografica == "Región") {
      selectInput(
        inputId = "SelectRegion",
        label = "Seleccionar Región:",
        choices = unique(agrupado_clinica$region),
        selected = "Centro"
      )
    } else if (area_geografica == "Jurisdicción") {
      selectInput(
        inputId = "SelectProv",
        label = "Seleccionar Jurisdicción:",
        choices = unique(agrupado_clinica$provincia_nombre),
        selected = "Buenos Aires"
      )
    } else {
      # Si no se selecciona "Región" ni "Jurisdicción", no mostrar nada.
      tagList()
    }
  })
  
  
  
  #activación de Incidencia
  observeEvent(input$TipoGrafico, { 
    if (input$TipoGrafico == "Columnas") {
      enable("Incidencia")
    } else if (input$TipoGrafico == "Curva Epidémica") {
      disable("Incidencia")
    }
  })
  
  
  #activación de SEPI
  observeEvent(input$TipoGrafico, { 
    if (input$TipoGrafico == "Columnas") {
      enable("SelectSEPI")
    } else if (input$TipoGrafico == "Curva Epidémica") {
      disable("SelectSEPI")
    }
  })
  
  
    
    #Barra de proceso de datos (modificar)
    
    #observeEvent(input$procesar, {
      
      # # inicia la barra de progreso
      # withProgress(message = "Procesando información...",{
      #   enable("limpiar")
      #   show("tabla", anim = T, animType = "fade")
      #   show("boxplot", anim = T, animType = "fade")
      #   
      #   # progreso 10%
      #   
      #   incProgress(0.1)
      #   
      #   # progreso 50%
      #   incProgress(0.5)
      #   Sys.sleep(2)
      #   output$tabla = DT::renderDataTable({
      #     
      #     datos = datosProcesados()
      #     DT::datatable(datos, caption = paste0("Especie seleccionada: ", input$selectSpecie))
      #   })
      #   
      #   Sys.sleep(2)
      #   output$boxplot = renderHighchart({
      #     #browser()
          
  
    
  datosFiltrados <- reactive({
    eventoSeleccionado <- input$SelectEvento
    anioSeleccionado <- input$SelectAnio
    semanaSeleccionada <- input$SelectSEPI
    
   
    if (input$SelectAreaGeografica == "Total Nacional") {
      datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio) %>%
        summarize(TotalCasos = sum(cantidad_casos))
    } else if (input$SelectAreaGeografica == "Región") {
      # Filtrar por región seleccionada
      regionSeleccionada <- input$SelectRegion
      datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, region == regionSeleccionada, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio) %>%
        summarize(TotalCasos = sum(cantidad_casos))
    } else if (input$SelectAreaGeografica == "Jurisdicción") {
      # Filtrar por jurisdicción seleccionada
      provinciaSeleccionada <- input$SelectProv
      datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, provincia_nombre == provinciaSeleccionada, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio) %>%
        summarize(TotalCasos = sum(cantidad_casos))
    }
    
    return(datosFiltrados)
  })
  
      
      output$graficoColumnas <- renderHighchart({
        datosGrafico <- datosFiltrados()
        
        if (!is.null(datosGrafico)) {
          print(datosGrafico)
          datos_json <- list(name = "Evento", data = as.list(datosGrafico$TotalCasos))
          hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = paste0("Notificaciones de ", input$SelectEvento, " en ", 
                                   input$SelectAreaGeografica, " hasta SE ", input$SelectSEPI)) %>% #corregir titulo
            hc_xAxis(categories = unique(datosGrafico$anio)) %>%
            hc_yAxis(title = list(text = "Cantidad de casos")) %>%
            hc_series(list(name = "Evento", data = datos_json$data, type = "column"))
          
          return(hc)
        }
      })
}
    








shinyApp(ui, server)