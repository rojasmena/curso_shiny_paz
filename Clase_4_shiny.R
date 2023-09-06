#Escribo shiny y selecciono del desplegable shinyapp o voy a file, new file,  shiny web app


library(shiny)

#interfaz usuario
ui <- fluidPage(
  
)


#server
server <- function(input, output, session) {
  
}


#llamada a la funcion shinyapp, le digo donde esta la interfaz de usuario y el server
shinyApp(ui, server)