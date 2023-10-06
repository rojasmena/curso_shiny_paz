library(shiny)
library(readxl)
library(tidyverse)
library(highcharter)
library(bslib)
library(shinyjs)

#Tema
TemaTrabajo=bs_theme(bootswatch = "cosmo")
TemaTrabajo=bs_theme_update(TemaTrabajo, fg = "#000", primary = "#27C8E3", 
                            secondary = "#F1418C", warning = "#E9FF18", font_scale = NULL, 
                            `enable-gradients` = TRUE, `enable-shadows` = TRUE, `enable-rounded` = TRUE, 
                            preset = "cosmo", bg = "#fff")

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


#  
  ui <- fluidPage(
      # titulo y tema
      useShinyjs(),
      theme=TemaTrabajo, 
      fluidRow(
        column(width = 12,
               h2(strong("Enfermedades Respiratorias Agudas")),
               align = "center"
        )
      ),
      
      hr(),
      
      #inputs####
      # primera Columnas de la ui
      fluidRow(
        column(width = 4,
                 selectInput(
                 inputId = "SelectEvento",
                 label = "Seleccionar Evento Clínico:",
                 choices = unique(agrupado_clinica$evento),
                 selected = "Enfermedad tipo influenza (ETI)"
               ),
               selectInput(
                 inputId = "SelectAreaGeografica",
                 label = "Seleccionar Área Geográfica:",
                 choices = c("Total Nacional", "Región","Jurisdicción") ,
                 selected = "Total Nacional"
               ),
              
               uiOutput("AreaGeograficaInputs"),
               selectInput(
                 inputId = "TipoGrafico",
                 label = "Gráfico:",
                 choices = c("Seleccionar", "Casos Acumulados", "Curva de Casos") ,
                 selected = "Seleccionar"
               ),
               checkboxInput(
                 "Incidencia",
                 "Mostrar tasas de incidencia"
               ),
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
                 "Limpiar filtro")
               )
        ),
       
        #inputs graficos####   
    #segunda columna     
        column(
          width = 8,
          align = "center",
          hr(),
          textOutput("titulo"),
          highchartOutput("graficoColumnas"),
          hidden(highchartOutput("graficoCurva")),
          hidden(highchartOutput("graficoCombinado")),
          hidden(textOutput("mensaje_2023"
                            )
                 )
          )
    )
  )
      
 

  
server <- function(input, output, session) {
 
  session$onSessionEnded(function() {
    stopApp()
  })
  
 #activación de inputs area geografica#####
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

      tagList()
    }
  })
  
 
  #activación de Incidencia####
  observeEvent(input$TipoGrafico, { 
    if (input$TipoGrafico == "Casos Acumulados" && input$SelectEvento!="Bronquiolitis en menores de 2 años") {
      enable("Incidencia")
    } else if (input$TipoGrafico == "Casos Acumulados" && input$SelectEvento=="Bronquiolitis en menores de 2 años") {
      disable("Incidencia")
    } else if (input$TipoGrafico == "Curva de Casos") {
      disable("Incidencia")
    }
  })
  
  observeEvent(input$SelectEvento, { 
    if (input$SelectEvento == "Bronquiolitis en menores de 2 años") { #se deshabilita para bronquiolitis porque no tengo el denominador
      disable("Incidencia")
    } else {
      tagList()
    }
  })
  

  #aparición de selectAnio####

  observeEvent(input$TipoGrafico, {
    if (input$TipoGrafico == "Curva de Casos") { 
        hide("SelectAnio", anim = T, animType = "fade")
    } else { 
      show("SelectAnio", anim = T, animType = "flip")
    }
  })
  
  
  observeEvent(input$TipoGrafico, {

    if (input$TipoGrafico == "Seleccionar") {
      hide("graficoColumnas", anim = T, animType = "fade")&
        disable("procesar")
    } else {
      enable("procesar")
    }

  })
  
  
 
  #reactive filtro de datos####  
  datosFiltrados <- reactive({
    eventoSeleccionado <- input$SelectEvento
    anioSeleccionado <- input$SelectAnio
    semanaSeleccionada <- as.numeric(input$SelectSEPI)
    graficoSeleccionado<-input$TipoGrafico
    regionSeleccionada <- input$SelectRegion
    provinciaSeleccionada <- input$SelectProv
    
   
    if (input$SelectAreaGeografica == "Total Nacional" & graficoSeleccionado== "Casos Acumulados") {
      datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio) %>%
        summarize(TotalCasos = sum(cantidad_casos, na.rm=T)) %>% 
        left_join((select(pobl,ano, pob, juri_nombre) %>% 
                     filter(juri_nombre=="TOTAL DEL PAÍS")), 
                  by=(c("anio"="ano"))) %>% 
        mutate(Incidencia=round((TotalCasos/pob)*100000, 2)) %>% 
        select(anio, TotalCasos, Incidencia)
      
    } else if (input$SelectAreaGeografica == "Región" & graficoSeleccionado== "Casos Acumulados") {
      # Filtrar por región seleccionada
        datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, region == regionSeleccionada, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
          group_by(anio) %>%
          summarize(TotalCasos = sum(cantidad_casos, na.rm=T))%>% 
          left_join((select(pobl,ano, pob, region, juri_nombre) %>% 
                       filter(region==regionSeleccionada) %>% 
                       group_by(ano) %>% 
                       summarise(pob=sum(pob, na.rm=T))), 
                    by=(c("anio"="ano"))) %>% 
          mutate(Incidencia=round((TotalCasos/pob)*100000, 2)) %>%  
          select(anio, TotalCasos, Incidencia)
        
    } else if (input$SelectAreaGeografica == "Jurisdicción" & graficoSeleccionado== "Casos Acumulados") {
      # Filtrar por jurisdicción seleccionada
        datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, provincia_nombre == provinciaSeleccionada, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
          group_by(anio) %>%
          summarize(TotalCasos = sum(cantidad_casos, na.rm=T))%>% 
          left_join((select(pobl,ano, pob, region, juri_nombre) %>% 
                       filter(juri_nombre==provinciaSeleccionada) %>% 
                       group_by(ano) %>% 
                       summarise(pob=sum(pob, na.rm=T))), 
                    by=(c("anio"="ano"))) %>% 
          mutate(Incidencia=round((TotalCasos/pob)*100000, 2)) %>% 
          select(anio, TotalCasos, Incidencia)
        
    } else if (input$SelectAreaGeografica == "Total Nacional" & graficoSeleccionado== "Curva de Casos") {
      # Filtrar por Total Nacional y hacer tabla para hc de curvas
      datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
      
    }else if (input$SelectAreaGeografica == "Región" & graficoSeleccionado== "Curva de Casos") {
      # Filtrar por región seleccionada y hacer tabla para hc de curvas
        datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, region == regionSeleccionada, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
        
    } else if (input$SelectAreaGeografica == "Jurisdicción" & graficoSeleccionado== "Curva de Casos") {
      # Filtrar por jurisdicción seleccionaday hacer tabla para hc de curvas
        datosFiltrados <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, provincia_nombre == provinciaSeleccionada, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
    }
    
    return(datosFiltrados)
  })
  
      #grafico Columnas#####
      output$graficoColumnas <- renderHighchart({
        datosGrafico <- datosFiltrados()
        
        if (!is.null(datosGrafico)) {
          print(datosGrafico)
          datos_json <- list(name = "Evento", data = as.list(datosGrafico$TotalCasos))
          hc <- highchart() %>%
            hc_chart(type = "column") %>% 
            hc_xAxis(categories = unique(datosGrafico$anio)) %>%
            hc_yAxis(title = list(text = "Cantidad de casos")) %>%
            hc_series(list(name = "Evento", data = datos_json$data, type = "column", color="#27C8E3"))%>%
            
            hc_exporting(enabled = TRUE) %>%
            hc_plotOptions(series = list(animation = TRUE))
          
          return(hc)
        }
      })
 
      #grafico combinado####
  output$graficoCombinado <- renderHighchart({
        datosCombinado <- datosFiltrados()
        evento<-input$SelectEvento
        
        if (!is.null(datosCombinado)) {
          print(datosCombinado)
          hc <- highchart()%>%
            hc_yAxis_multiples(list(title=list(text= "Cantidad de casos",
                                           style=list(fontWeight="bold", fontSize="auto")),
                                labels=list(style=list(fontSize="auto", color="black")),
                                showFirstLabel=TRUE,showLastLabel=TRUE, opposite=FALSE),
                               list(title=list(text= "Tasa de Incidencia",
                                               style=list(fontWeight="bold", fontSize="auto", color="#F1418C")),
                                    labels=list(style=list(fontSize="auto", color="black")),
                                    showLastLabel=TRUE, opposite=TRUE)) %>%
            hc_plotOptions(column=list(stacking="normal"))  %>%
            hc_add_series(datosCombinado, 
                      type = 'column',
                      name = paste0("Casos de ", evento),
                      hcaes(x=anio, y=TotalCasos),
                      yAxis=0,
                      color="#27C8E3", 
                      dataLabeles=list(enable=TRUE, 
                                       style=list(fontsize="auto"), 
                                       color= "#27C8E3"))%>%
            hc_add_series(datosCombinado, 
                          type = 'line',
                          name = paste0("Tasa de Incidencia cada 100.000 hab"),
                          hcaes(x=anio, y=Incidencia),
                          yAxis=1,
                          color="#F1418C", 
                          dataLabeles=list(enable=TRUE, 
                                           style=list(fontsize="auto"), 
                                           color= "#F1418C"))%>%
            hc_xAxis(categories = unique(datosCombinado$anio)) %>%
            
            hc_exporting(enabled= TRUE)
          
          return(hc)
        }
      })
      
      
      #grafico curvas#### 
     output$graficoCurva <- renderHighchart({
        datosCurva <- datosFiltrados()
        colnames(datosCurva) <- c("semanas_epidemiologicas", "a2019", "a2020", "a2021", "a2022", "a2023")
        
        if (!is.null(datosCurva)) {
          hc <- highchart() %>%
            hc_chart(type = 'area') %>%
            hc_xAxis(categories = datosCurva$semanas_epidemiologicas,
                     title = list(text = "Semanas Epidemiológicas")) %>%
            hc_yAxis(
              title = list(text = "Casos")
            ) %>%
            
               hc_add_series(
                 datosCurva$a2019,
                 name = 'Año 2019',
                 dataLabels = list(enabled = FALSE),
                 color = "#27C8E3"
               ) %>%
               hc_add_series(
                 datosCurva$a2020,
                 name = 'Año 2020',
                 dataLabels = list(enabled = FALSE),
                 color = "#e41a1c") %>%
             hc_add_series(
               datosCurva$a2021,
               name = 'Año 2021',
               dataLabels = list(enabled = FALSE),
               color = "#66a61e") %>%
             hc_add_series(
               datosCurva$a2022,
               name = 'Año 2022',
               dataLabels = list(enabled = FALSE),
               color = "#F1418C") %>%
              hc_add_series(
                datosCurva$a2023,
                name = 'Año 2023',
                dataLabels = list(enabled = FALSE),
                color = "#E9FF18") %>%
             hc_exporting(enabled = TRUE) %>%
             hc_plotOptions(series = list(animation = TRUE))
          
          return(hc)
        }
      })      

  
  
  #output titulo####
 
  output$titulo = renderText({
    eventoSeleccionado <- input$SelectEvento
    semanaSeleccionada <- as.numeric(input$SelectSEPI)
    graficoSeleccionado<-input$TipoGrafico
    area_geografica <- input$SelectAreaGeografica
    regionSeleccionada <- input$SelectRegion
    provinciaSeleccionada <- input$SelectProv
    procesarClick<-input$procesar
    
    
    if (graficoSeleccionado== "Curva de Casos" && procesarClick) {
      paste0("Gráfico: Casos de ", eventoSeleccionado, " por semana epidemiológica en ", ifelse(
        area_geografica=="Región", regionSeleccionada, ifelse(
          area_geografica=="Jurisdicción", provinciaSeleccionada, "Argentina")), ", SE 1 a ", semanaSeleccionada)
    } else if (graficoSeleccionado== "Casos Acumulados"  && input$Incidencia && eventoSeleccionado!="Bronquiolitis en menores de 2 años" && procesarClick) {
      paste0("Gráfico: Casos de ", eventoSeleccionado, " acumulados y tasa de incidencia cada 100.000 hab. en ", ifelse(
        area_geografica=="Región", regionSeleccionada, ifelse(
          area_geografica=="Jurisdicción", provinciaSeleccionada, "Argentina")), ", SE 1 a ", semanaSeleccionada)
    } else if (graficoSeleccionado== "Casos Acumulados" && procesarClick) {
    paste0("Gráfico: Casos de ", eventoSeleccionado, " acumulados en ", ifelse(
      area_geografica=="Región", regionSeleccionada, ifelse(
        area_geografica=="Jurisdicción", provinciaSeleccionada, "Argentina")), ", SE 1 a ", semanaSeleccionada)
    } else if (graficoSeleccionado== "Seleccionar") {
      "Seleccione filtros para generar el gráfico"
    }
    
})
  
  #output mensaje alerta 2023####
  output$mensaje_2023 = renderText({
    "Para el año 2023 solo se dispone de información hasta SE26 inclusive"
  })
  

 
     #observe para boton procesar####

      observeEvent(input$procesar, {
        shinyjs::show(if (input$TipoGrafico== "Curva de Casos") {
           "graficoCurva"
           
         } else if (input$TipoGrafico== "Casos Acumulados" && input$Incidencia  && input$SelectEvento!= "Bronquiolitis en menores de 2 años") {
           "graficoCombinado"
           
         } else if (input$TipoGrafico== "Casos Acumulados") {
           "graficoColumnas"
         })
        
       shinyjs::show("titulo")
       shinyjs::show(if ("2023" %in% input$SelectAnio) {
         "mensaje_2023"})
       enable("limpiar")
       disable("procesar")
       disable("TipoGrafico")
       disable("SelectEvento")
       disable("SelectAreaGeografica")
       disable("Incidencia")
       disable("SelectAnio")
       disable("SelectSEPI")
       disable ("SelectRegion")
       disable("SelectProv")


     })



     observeEvent(input$limpiar, {
       disable("limpiar")
       enable("TipoGrafico")
       enable("procesar")
       enable("SelectEvento")
       enable("SelectAreaGeografica")
       enable("SelectAnio")
       enable("SelectSEPI")
       enable ("SelectRegion")
       enable("SelectProv")
       enable("Incidencia")
       shinyjs::hide("titulo")
       shinyjs::hide("mensaje_2023")
       shinyjs::hide("graficoCurva")
       shinyjs::hide("graficoCombinado")
       shinyjs::hide("graficoColumnas")
     })

      

}
 
shinyApp(ui, server)