library(shiny)
library(DT)
library(tidyr)
library(glue) #para los textos, usando {}, 
library(highcharter)
library(shinyjs) #tiene funciones pre cargadas de java script
library(clipr)

datos = iris
datos$Species = toupper(datos$Species)

ui <- fluidPage(
  # titulo
  useShinyjs(),
  fluidRow(
    column(width = 12,
           h1(strong("Título de la aplicación")),
           align = "center"
    )
  ),
  
  hr(),
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
           
           h2("Filtros"),
           selectInput(
             inputId = "selectSpecie",
             label = "Seleccionar especie:",
             choices = unique(datos$Species),
             selected = "VERSICOLOR"
           ),
           
           actionButton(
             "boton",
             "Ver nro filas"
           ),
           actionButton(
             "clip",
             "copiar datos"
           ),
           checkboxInput(
             inputId = "MOSTRAR_BOXPLOT",
             label="Mostrar/ocultar boxplot",
             value=TRUE
           ),
           
           align = "center"
    ),
    column(width = 9,
           h2("Título 2 de la col 2"),
           hr(),
           DT::DTOutput("tabla"),
           br(), #espacio en blanco
           highchartOutput("boxplot"), #output llamado boxplot
           textOutput("muestra_mensaje"),
           align = "center"
    )
    
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$MOSTRAR_BOXPLOT,{ #estas son funciones de shinyjs
    if(input$MOSTRAR_BOXPLOT==T){
      show("boxplot")
    } else {
      hide("boxplot")
    }
  })
  
  observeEvent(input$clip,{
    clipr::write_clip(datosProcesados())
  }) #le digo que haga una actividad que no impacta en el output. Podria ser tb una descarga a excel. 
  
  #el bloque reactive procesa datos que dps pueden ser llamados por todos los outputs
  #si proceso info en cada output puede ser repetitivo y enlentecer el proceso
  datosProcesados = reactive({
    especieSeleccionada = input$selectSpecie
    datos = datos[datos$Species == especieSeleccionada,]
    datos
  })
  
 #eventReactive
   mensaje = eventReactive(input$boton,{
    filas = nrow(datosProcesados())
    glue("cantidad de filas {filas} ")
  })
  
  
  
  output$tabla = DT::renderDataTable({
    
    datos = datosProcesados()
    DT::datatable(datos, caption = paste0("Especie seleccionada: ", input$selectSpecie))
    
  })
  
  output$boxplot= renderHighchart({
    datos = datosProcesados()
    
    datos = datos %>% pivot_longer(cols = 1:4, values_to = "Valores", names_to = "Medidas")
    datos$Species = NULL
    
    datos$Medidas = factor(datos$Medidas)
    datos = datos %>% as_tibble()
    
    # grafico
    data_boxplot=data_to_boxplot(
      data = datos,
      variable = Valores,
      group_var = Medidas,
      group_var2 = Medidas,
      add_outliers = F,
      fillColor = c('#a6611a','#dfc27d','#80cdc1','#018571'),
      color="black"
    )
    
    highchart()%>%
      hc_xAxis(type ="category")%>%
      hc_add_series_list(data_boxplot)%>%
      hc_xAxis(title = list(text = "Medida"))%>%
      hc_yAxis(title = list(text = "Centímetros"))%>%
      hc_title(text= glue("Boxplot para medidas de <em>{input$selectSpecie}</em>")) %>% #usa glue, le pone cursiva. Si reemplazo em por b le pone negrita
      hc_subtitle(text= "Medidas de pétalo y sépalo") %>%
      hc_legend(enabled= FALSE)
  })
  
  
  


#output mensaje filas
output$muestra_mensaje = renderText({
  mensaje()
})

}
shinyApp(ui, server)
