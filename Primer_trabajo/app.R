#Paquetes

instalar = function (libreria) {
  if (libreria %in% installed.packages()[,"Package"]) {
    eval(parse(text=paste0("library(",libreria,")")))} else {
      install.packages(libreria)    
      eval(parse(text=paste0("library(",libreria,")")))
      library(libreria)
    }
}

instalar("dplyr")
instalar("shiny")
instalar("tidyverse")
instalar("tidyr")
instalar("DT")
instalar("lubridate")
instalar("shinyWidgets")
instalar("highcharter")

#descargo el dataset 

options(timeout=1000000) # incrementamos el timeout debido a que la descarga es lenta

url = "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv"

download.file(url, destfile = "TMI.csv")

data = read.csv("TMI.csv")
unlink("TMI.csv")

DT::datatable(data)

###proceso los datos para graficar
bd <- data %>% pivot_longer(
  cols = !indice_tiempo,
  names_to = "prov",
  values_to = "TMI"
) %>%
  mutate(
    prov = str_sub(prov, 21, nchar(prov)),
    ano = year(ymd(indice_tiempo)),
    indice_tiempo = ymd(indice_tiempo),
    prov=case_when(prov == "cordoba" ~ str_to_title(prov),
                   prov == "caba"  ~ "CABA",
                   prov == "argentina" ~ str_to_title(prov),
                   prov == "corientes" ~ str_to_title(prov),
                   prov == "chaco" ~ str_to_title(prov),
                   prov == "chubut" ~ str_to_title(prov),
                   prov == "neuquen" ~ str_to_title(prov),
                   prov == "misiones" ~ str_to_title(prov),
                   prov == "jujuy" ~ str_to_title(prov),
                   prov == "catamarca" ~ str_to_title(prov),
                   prov == "corrientes" ~ str_to_title(prov),
                   prov == "formosa" ~ str_to_title(prov),
                   prov == "salta" ~ str_to_title(prov),
                   prov == "mendoza" ~ str_to_title(prov),
                   prov == "tucuman" ~ str_to_title(prov),
                   prov == "buenosaires" ~ "Buenos Aires",
                   prov == "santiagodelestero" ~ "Santiago del Estero",
                   prov == "santafe" ~ "Santa Fe",
                   prov == "tierradelfuego" ~ "Tierra del Fuego",
                   prov == "santacruz" ~ "Santa Cruz",
                   prov == "sanjuan" ~ "San Juan",
                   prov == "sanluis" ~ "San Luis",
                   prov == "lapampa" ~ "La Pampa",
                   prov == "larioja" ~ "La Rioja",
                   prov == "entrerios" ~ "Entre Rios",
                   prov == "rionegro" ~ "Rio Negro",
                   TRUE ~ prov  # Mantén el valor original para otros casos
    )
  ) %>% 
  select(-indice_tiempo)

DT::datatable(bd)

#

ui <- fluidPage(
  fluidRow(
    column(width=12,
           tags$h1(strong("Tasa de Mortalidad Infantil en Argentina")),
           align= "center"
           )
    ),
  hr(),
 
   #primera fila de la ui
  fluidRow(
    column (width=2,
            checkboxGroupInput(
              inputId = "selectProvincia",   
              label= "Seleccionar provincias de interés:",   
              choices = unique(bd$prov),
              selected="Argentina"
            ),
            align= "left"
    ),                       
    column (width=10,
            tags$h4("Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, período 1990-2021"),
            highchartOutput("Grafico"),
            align= "left"
    )
  )
)


server <- function(input, output, session) {
  output$Grafico= renderHighchart({ 
  provSeleccionada= input$selectProvincia
# armo el grafico con highchart
hc <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021") %>%
  hc_xAxis(title = list(text = "Año")) %>%
  hc_yAxis(title = list(text = "TMI ")) %>%
  hc_exporting(enabled = TRUE) 


# Agrega una serie de datos para cada nivel de "prov"
for (nivel in niveles_prov) {
  data_serie <- bd[bd$prov == nivel,]
  hc <- hc %>%
    hc_add_series(
      data_serie,
      "line",
      hcaes(x = ano, y = TMI),
      name = nivel,
      marker = list(radius = 4)
    )}})
}
hc

shinyApp(ui, server)
