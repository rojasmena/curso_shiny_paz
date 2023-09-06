#Escribo shiny y selecciono del desplegable shinyapp o voy a file, new file,  shiny web app


library(shiny)
library(DT)

#interfaz usuario
datos<-iris
datos$Species<-toupper(datos$Species)

ui <- fluidPage( #en la primera funcion le digo la interfaz de ususario. Fluidpage es como una cuadricula de filas y 12 columnas y me permite administrar mas libremente la disposición de la información. #tiene logica de cuadricula. 
#titulo
    fluidRow(#con fluidRow genero una primera fila 
    column(width=12, #ancho 12 (ocupa toda la página)
           br(), #dejo una línea en blanco. No es recomendable, lo que debería hacer es cambiar márgenes. 
           em(tags$h1(strong("Título de la Aplicación"))), 
           br(),
           align= "center" #titulo mas grande, de la mayor jerarquia y centrado #em indica cursiva #strong para negrita
            )
  ), 
  
  hr(), #le pongo una línea entre filas
  #primera fila de la ui
  fluidRow(
    column (width=3,                          #ancho 6, ocupa la mitad de la página
            tags$h2("FILTROS"),
            selectInput(
              inputId = "selectSpecie",       #es el nombre que voy a usar dps en el server, es recomendable usar siempre la misma estructura
              label= "Seleccionar especie",   #etiqueta 
              choices = unique(datos$Species), #valores de la base, pero puedo poner c("SETOSA"="setosa", "VERSICOLOR"="versicolor")--El primer valor es el que ve el usuario y el segundo es el que tiene la base. #lo mas recomendable es usar unique
              selected="SETOSA"
            ),
            align= "center"
            ),                                  #puedo ir agregando filas al interior de las columnas, con p() introduzco párrafos. 
    column (width=9,
            tags$h2("Título 2 de la columna 2"),
            DT::DTOutput("tabla"),
            align= "center"
    )
  )
)


#server
server <- function(input, output, session) {  #input/output/session siempre tienen q estar. 
  output$tabla= DT::renderDataTable({         #voy a renderizar las tablas para que se vean en shiny, tengo que abrir parentesis y llave
  especieSeleccionada= input$selectSpecie  
  datos=datos[datos$Species==especieSeleccionada,]
  DT::datatable(datos, caption = paste0("Especie seleccionada: ",especieSeleccionada))
  }) 
    
}


#llamada a la funcion shinyapp, le digo donde esta la interfaz de usuario y el server
shinyApp(ui, server)

#cuando guardo el archivo, me aparece RUN APP y la opcion publish
#RUNAPP PUEDE SER EN EL EXPLORADOR/ventana/viewer. 
#para seguir trabajando tengo que apretar stop en la consola.