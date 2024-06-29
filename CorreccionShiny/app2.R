library(shiny)
library(readxl)
library(tidyverse)
library(highcharter)
library(bslib)
library(shinyjs)
print(getwd())
load("resultado.RData")
agrupado_clinica = resultado$agrupado_clinica
datos = resultado$datos
datos1 = resultado$datos1
datos2 = resultado$datos2
datos3 = resultado$datos3
pobl = resultado$pobl
rm(resultado)

#Tema
TemaTrabajo=bs_theme(bootswatch = "cosmo")
TemaTrabajo=bs_theme_update(TemaTrabajo, fg = "#000", primary = "#27C8E3", 
                            secondary = "#F1418C", warning = "#E9FF18", font_scale = NULL, 
                            `enable-gradients` = TRUE, `enable-shadows` = TRUE, `enable-rounded` = TRUE, 
                            preset = "cosmo", bg = "#fff")


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
    if (input$SelectEvento == "Bronquiolitis en menores de 2 años" || input$TipoGrafico != "Casos Acumulados") {
      disable("Incidencia")
    } else {
      enable("Incidencia")
    }
  })
  
  observeEvent(input$SelectEvento, { 
    if (input$SelectEvento == "Bronquiolitis en menores de 2 años" || input$TipoGrafico != "Casos Acumulados") {
      disable("Incidencia")
    } else {
      enable("Incidencia")
    }
  })
  
  
  #aparición de selectAnio####
  
  observeEvent(input$TipoGrafico, {
    if (input$TipoGrafico == "Curva de Casos") { 
      hide("SelectAnio", anim = T, animType = "fade")
    } else { 
      shinyjs::show("SelectAnio", anim = T, animType = "flip")
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
      datosReturn <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, anio %in% anioSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio) %>%
        summarize(TotalCasos = sum(cantidad_casos, na.rm=T)) %>% 
        left_join((pobl %>% dplyr::select(ano, pob, juri_nombre) %>% 
                     filter(juri_nombre=="TOTAL DEL PAÍS")), 
                  by=(c("anio"="ano"))) %>% 
        mutate(Incidencia=round((TotalCasos/pob)*100000, 2)) %>% 
        select(anio, TotalCasos, Incidencia)
      
    } else if (input$SelectAreaGeografica == "Región" & graficoSeleccionado== "Casos Acumulados") {
      # Filtrar por región seleccionada
      datosReturn <- agrupado_clinica %>%
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
      datosReturn <- agrupado_clinica %>%
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
      datosReturn <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
      
    }else if (input$SelectAreaGeografica == "Región" & graficoSeleccionado== "Curva de Casos") {
      # Filtrar por región seleccionada y hacer tabla para hc de curvas
      datosReturn <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, region == regionSeleccionada, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
      
    } else if (input$SelectAreaGeografica == "Jurisdicción" & graficoSeleccionado== "Curva de Casos") {
      # Filtrar por jurisdicción seleccionaday hacer tabla para hc de curvas
      datosReturn <- agrupado_clinica %>%
        filter(evento == eventoSeleccionado, provincia_nombre == provinciaSeleccionada, semanas_epidemiologicas <= semanaSeleccionada) %>%
        group_by(anio, semanas_epidemiologicas) %>%
        summarise(CASOS= sum(cantidad_casos, na.rm=TRUE)) %>% 
        spread(anio, CASOS)
    }
    if (exists("datosReturn")) {
      return(datosReturn)
    } else {}
    
  })
  
  #grafico Columnas#####
  output$graficoColumnas <- renderHighchart({
    
    datosGrafico <- datosFiltrados()
    if (!is.null(datosGrafico)) {
      
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
          color = "#27C8E3") %>%
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
