library(shiny)
library(DT)
library(tidyr)
library(glue) #para los textos, usando {}, 
library(highcharter)
library(shinyjs) #tiene funciones pre cargadas de java script
library(clipr)
library(bslib) #para personalizar los temas de estilo. corriendo bslib::bs_theme_preview() en la consola tira ejemplos de como quedan los estilos
library(shinythemes) #libreria para definir estilos. Los temas estan disponibles en https://rstudio.github.io/shinythemes/

datos = iris
datos$Species = toupper(datos$Species)
tema_untref= bs_theme(bootswatch = "cerulean") #guardo el tema como un objeto, usando la libreria bslib
tema_untref = bs_theme_update(tema_untref, bg = "#fff", base_font = font_google("Montserrat"), 
                              fg = "#000")


ui <- fluidPage(
  # titulo
  useShinyjs(),
  theme=tema_untref, #asi se programa el tema con la libreria bslib
 # theme= shinytheme("flatly"), #aca le puedo meter el tema usando la librerya shinytheme
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
           disabled(checkboxInput(
             inputId = "MOSTRAR_BOXPLOT",
             label="Mostrar/ocultar boxplot",
             value=TRUE
           )),
           br(),
           actionButton(
             "procesar",
             "Procesar"
           ),
           disabled(actionButton(
             "limpiar",
             "Limpiar filtro"
           )),
           br(),
           hidden(
             downloadButton( #BOTON DE DESCARGA
             "descargar",
             "Descargar"
           )),
           
           align = "center"
    ),
    column(width = 9,
           h2("Tabla"),
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
  #bslib::bs_themer() #esta linea me permite modificar el tema en la shiny a mano y despuees incorporar al codigo
    observeEvent(input$MOSTRAR_BOXPLOT,{ #estas son funciones de shinyjs
    if(input$MOSTRAR_BOXPLOT==T){
      show("boxplot", anim=T, animType="fade")
    } else {
      hide("boxplot", anim=T, animType="fade")
    }
  })
  
  observeEvent(input$clip,{  #le digo que haga una actividad que no impacta en el output. Podria ser tb una descarga a excel. 
    clipr::write_clip(datosProcesados())
    showNotification("Copiado!", type = "message")
  })
  
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
  
  
    observeEvent(input$procesar, {
      show("tabla", anim=T, animType="fade")
      show("boxplot", anim=T, animType="fade") #animaciones para aparecer, ver tipos
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
      disable("selectSpecie") #funcion de shinyjs que deshabilita opciones
      disable("procesar")
      enable("limpiar")
      enable("MOSTRAR_BOXPLOT")
      show("descargar", anim=T, animType="fade")
  })
    
    observeEvent(input$limpiar, {
      hide("tabla")
      hide("boxplot", anim=T, animType="fade")
      enable ("procesar")
      enable ("selectSpecie")
      disable("limpiar")
      disable("MOSTRAR_BOXPLOT")
      hide("descargar", anim=T, animType="fade")
      
    }) 
  
    #output mensaje filas
  output$muestra_mensaje = renderText({
    mensaje()
  })
  
  #output descarga
  output$descarga=downloadHandler( #funcion para la descarga. Una funcion tiene el nombre de la descarga y la otra los datos que se descargan. Puedo incorporar algo de procesamiento previo a la descarga
    browser()
    datos_descarga=datosProcesados
  )
  
}
shinyApp(ui, server)

