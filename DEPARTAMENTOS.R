instalar = function (libreria) {
  if (libreria %in% installed.packages()[,"Package"]) {
    eval(parse(text=paste0("library(",libreria,")")))} else {
      install.packages(libreria)    
      eval(parse(text=paste0("library(",libreria,")")))
      library(libreria)
    }
}

instalar("dplyr")
instalar("glue")
instalar("foreign")
instalar("stringr")

#debemos descargar la cartografía de Argentina dividida en departamentos
url = "https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip"
download.file(url, destfile = "mapa_deptos.zip")


#DESCOMPRIMIR

unzip("mapa_deptos.zip",exdir = "mapa_deptos")
unlink("mapa_deptos.zip")


#Ahora vamos a buscar el nombre del archivo .dbf que contiene la carpeta e importarlo en un data frame.

nombres_archivos = list.files("mapa_deptos")
nombre_dbf = nombres_archivos[grep("dbf",nombres_archivos)]
codigos = foreign::read.dbf(glue("mapa_deptos/{nombre_dbf}"))


#La columna ‘link’ contiene la información que necesitamos. Las primeras dos posiciones representan el código de la jurisdicción y las tres siguientes el del departamento. Generamos la tabla de códigos, incluyendo los nombres de jurisdicción y departamento.
codigos = data.frame(
  juri_codigo = substring(codigos$link,1,2),
  juri_nombre = codigos$provincia,
  departamento_codigo = substring(codigos$link,3,5),
  departamento_nombre = codigos$departamen) %>% arrange (juri_codigo,departamento_codigo)
head(codigos)


# eliminar la palabra “Comuna” en los nombres de departamentos de la CABA, para representarlos de la misma forma que en la otra fuente de datos.
#incidir el nombre asignado a la provincia de Tierra del Fuego y al departamento Ñorquincó de Neuquén.

codigos$departamento_nombre = str_replace_all(codigos$departamento_nombre, "Comuna ","")
codigos$juri_nombre = as.character(codigos$juri_nombre)
codigos$juri_nombre[codigos$juri_nombre=="Tierra del Fuego"] = "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
codigos$departamento_nombre[codigos$departamento_nombre=="Ñorquinco"] = "Ñorquincó"


#Las proyecciones por departamento se presentan en 24 archivos (uno para cada jurisdicción). En primer lugar vamos a descargar los archivos pogramáticamente. 

#Extraer links de una página web es una tarea relativamente fácil que puede hacerse con el paquete rvest. Por ejemplo, podemos extraer todos los links de la página principal de la UNTREF.

instalar("rvest")
url = 'https://untref.edu.ar/'
pagina_web = read_html(url)
links = pagina_web %>% html_nodes("a") %>% html_attr("href") 
links = links[substring(links,1,4)== "http" & is.na(links)==F] 
head(links)


#Para el caso de la web del INDEC esta metodología sencilla no funciona, ya que los links aparecen ocultos 

#librería webdriver que nos permite ejecutar un navegador virtual que cargue la página por completo y extraiga el código html tal cual lo muestra el navegador

instalar("webdriver")


#install_phantomjs() # sólo correr esta línea la primera vez que se use webdriver
pjs <- run_phantomjs()
ses <- Session$new(port = pjs$port)

# inicia una sesión el navegador virtual
ses$go("https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-119")
Sys.sleep(5)

# obtiene el código fuente de la página web
codigo_html=ses$getSource()

# aplica funciones de procesamiento de texto para "limpiar" los links
links = strsplit(codigo_html,"\n")[[1]][grep("ftp",strsplit(codigo_html,"\n")[[1]])]
links = unlist(strsplit(links,"<"))[substring(unlist(strsplit(links,"<")),1,7)=="a class"]
links = unlist(strsplit(links,"/"))[substring(unlist(strsplit(links,"/")),1,4)=="proy"]
links1 = unlist(strsplit(links," "))[substring(unlist(strsplit(links," ")),1,4)=="proy"]
links1 = paste0("https://www.indec.gob.ar/ftp/cuadros/poblacion/",stringr::str_remove_all(links1,'\"'))
links = unlist(strsplit(links,">"))[substring(unlist(strsplit(links,">")),1,4)!="proy"]
links = gsub("\\s+$", "", links)

# genera un data frame con los links al archivo de cada jurisdicción
links = data.frame(juri = links, links=links1)
head(links)



#Creamos una carpeta llamada ‘archivos_proyecciones’ donde vamos a alojar los 24 archivos descargados.

dir.create("archivos_proyecciones")

for (i in 1:nrow(links)) {
  jurisdiccion = str_replace_all(links[i,1],".xls","")
  download.file(links[i,2], dest = glue("archivos_proyecciones/{jurisdiccion}.xls"), mode="wb")
}

#Con los archivos descargados, vamos a generar un loop que importe cada uno y extraiga la información para almacenarla en un data frame. Para eso comenzaremos con generar un vector con todas las rutas a los archivos que importaremos.

rutas_a_archivos = paste0("archivos_proyecciones/",list.files("archivos_proyecciones"))


instalar("readxl")
proyecciones_depto = data.frame()

for (i in 1:length(rutas_a_archivos)) { # recorre los 24 archivos descargados
  
  # inicializa variable
  incluir=T
  
  # recorre las columna A de cada archivo y obtiene el largo de cada bloque
  for (j in 9:500) {
    prov = str_remove_all(rutas_a_archivos[i],"archivos_proyecciones/")
    prov = str_remove_all(prov,".xls")
    comienzo_bloque_1 = if (prov %in% c("Buenos Aires","La Rioja", "Río Negro","Salta", "San Juan",
                                        "San Luis","Santa Cruz","Santa Fe","Santiago del Estero",
                                        "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                                        "Tucumán")) {11} else {10}
    celda = comienzo_bloque_1-1+j
    valor=as.character(read_xls(rutas_a_archivos[i], range = glue("A{celda}:A{celda}"), col_names = F))
    if (length(valor)>0) {if(valor=="Total") {break}} 
  }
  
  # incluye parches para archivos que varían con formatos irregulares
  if (prov == "Buenos Aires") {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -8
    comienzo_bloque_3 = fin_bloque_2 + 10
    fin_bloque_3 = comienzo_bloque_3 + j -8
  } else if (prov == "La Pampa") {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -9
    comienzo_bloque_3 = fin_bloque_2 + 11
    fin_bloque_3 = comienzo_bloque_3 + j -9
  } else if (prov %in% c("La Rioja","Río Negro","Salta")) {
    fin_bloque_1 = comienzo_bloque_1 + j -10
    comienzo_bloque_2 = fin_bloque_1 + 11
    fin_bloque_2 = comienzo_bloque_2 + j -10
    comienzo_bloque_3 = fin_bloque_2 + 11
    fin_bloque_3 = comienzo_bloque_3 + j -10
  } else {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -9
    comienzo_bloque_3 = fin_bloque_2 + 10 
    fin_bloque_3 = comienzo_bloque_3 + j -9
  }
  
  # da formato a los datos capturados
  datos = rbind(
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_1}:Q{fin_bloque_1}"), col_names = F),
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_2}:Q{fin_bloque_2}"), col_names = F),
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_3}:Q{fin_bloque_3}"), col_names = F)
  )
  
  colnames(datos) = paste0("a_",2010:2025)
  datos = datos[is.na(datos$a_2010)==F,]
  datos$juri_nombre = prov
  datos$sexo_nombre = c(rep("Ambos sexos", nrow(datos)/3),
                        rep("Varones", nrow(datos)/3),
                        rep("Mujeres", nrow(datos)/3))
  departamento_nombre = unique(read_xls(rutas_a_archivos[i], range = glue("A{comienzo_bloque_1}:A{fin_bloque_3}"), col_names = F))
  departamento_nombre = departamento_nombre[is.na(departamento_nombre)==F]
  departamento_nombre = departamento_nombre[departamento_nombre!="Interior de la Provincia"]
  departamento_nombre = departamento_nombre[departamento_nombre!="24 Partidos del GBA"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Varones"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Mujeres"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Total"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Partido"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Departamento"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Comuna"]
  
  datos$departamento_nombre = rep(departamento_nombre,3)
  datos = datos[,c("juri_nombre","departamento_nombre", "sexo_nombre",colnames(datos)[substring(colnames(datos),1,2)=="a_"])]
  
  # añade la información capturada de cada jurisdicción al archivo final
  proyecciones_depto = rbind(
    proyecciones_depto,
    datos
  )
}

#Verificamos que el procedimiento verificando que la información del año 2010 para el total del país coincide con la que se observa en el archivo de proyecciones provinciales

url_prov = "https://www.indec.gob.ar/ftp/cuadros/poblacion/c2_proyecciones_prov_2010_2040.xls"

# descarga el archivo de las proyecciones jurisdiccionales a través de la línea de comandos de Windows debido a que la función download.file suele fallar con archivos .xls
system(glue('curl -o poblacion.xls {url_prov}')) #ESTA LINEA NO ME FUNCIONA, DESCARGUE EL ARCHIVO MANUALMENTE Y LE ASIGNE EL NOMBRE POBLACION


sheets = readxl::excel_sheets("poblacion.xls")

# valores provenientes de las proyecciones por jurisdicción
total_argentina_2010_prov = as.numeric(colnames(read_xls("poblacion.xls", sheet= sheets[2],range = "B6:D6", col_names = T)))
names(total_argentina_2010_prov) = c("Ambos sexos","Varones","Mujeres")

# valores provenientes de las proyecciones por departamentos
total_argentina_2010_depto = c(ambos_sexos=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Ambos sexos"]),
                               varones=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Varones"]),
                               mujeres=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Mujeres"]))

# chequea que ambas fuentes coincidan
total_argentina_2010_depto == total_argentina_2010_prov


#Vamos a utilizar ahora la tabla de códigos que generamos anteriormente para vincularla a través de los nombres de las áreas geográficas de los datos procesados y recuperar los códigos.
# vincula ambas fuentes
proyecciones_depto = left_join(proyecciones_depto,codigos, by = c("juri_nombre","departamento_nombre"))

# Verifica que no haya quedado algún departamento sin código asignado
nrow(proyecciones_depto[is.na(proyecciones_depto$departamento_codigo),])

# agrega códigos de sexo
proyecciones_depto$sexo_codigo = ""
proyecciones_depto$sexo_codigo[proyecciones_depto$sexo_nombre == "Ambos sexos"] = "0"
proyecciones_depto$sexo_codigo[proyecciones_depto$sexo_nombre == "Varones"] = "1"
proyecciones_depto$sexo_codigo[proyecciones_depto$sexo_nombre == "Mujeres"] = "2"

# pasa años a filas
instalar("tidyverse")
proyecciones_depto$a_2025 = as.numeric(proyecciones_depto$a_2025) # pasa a numérica ya que se almacena originalmente como character debido al formato de los archivos originales

proyecciones_depto = proyecciones_depto %>% pivot_longer(cols = 4:19, names_to = "ano", values_to = "poblacion")

instalar("stringr")
proyecciones_depto$ano = str_replace_all(proyecciones_depto$ano,"a_","") # elimina prefijo "a_"

instalar("DT")
DT::datatable(proyecciones_depto)


write.csv2(proyecciones_depto, "proyecciones_depto.csv",  na = "", fileEncoding="latin1")
