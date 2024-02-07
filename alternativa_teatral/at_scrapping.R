library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(robotstxt)

#setwd("~/Documentos/Mapa de los estadios")


paths_allowed(
  paths = c("http://www.alternativateatral.com.ar/teatros.asp")
) #TODO JOYA






########################## INICIO ####################################
#Creo Data Frame vacio

df <- data.frame(        link=character(),
                         nombre=character(), 
                         direccion=character(),
                         localidad=character(),
                         provincia=character(),
                         pais=character(),
                         telefono=character(),
                         google_link=character(),
                         coordenadas=character(),
                         web_oficial=character()
)


web <- "http://www.alternativateatral.com.ar/"

# Leer el HTML de la página web
pagina <- read_html("http://www.alternativateatral.com.ar/teatros.asp?pais=1")


#Para extraer las provincias



lista <- html_nodes(pagina, "#provincias li a")
valores <- html_text(lista) #nombre de las prov
hrefs <- html_attr(lista, "href") #links a las provincias
df <- data.frame(valor = valores, href = hrefs)



# Iteración en provincias para sacar zonas

zonas <- paste0(web,hrefs[1]) # el 1 va ir iterando

pagina_provincias <- read_html(zonas)


#Para extraer las zonas


lista_li <- html_nodes(pagina_provincias, "#zonas li a")

hrefs_zonas <- html_attr(lista_li, "href") #links a las provincias
text_zonas <- html_text(lista_li)

pagina_zonas <- paste0(web,hrefs_zonas[1]) # el 1 va ir iterando


#Extraer data para luego ir a los espacios


pagina_espacios <- read_html(pagina_zonas)
tds <- html_nodes(pagina_espacios, "td.celdadatos a") #para ver los links


# Extraer el atributo href de los elementos <td>
hrefs_espacios <- html_attr(tds, "href")#links a la web... el largo sera la q de espacios

#Espacios

espacio <- paste0(web,hrefs_espacios[1])# el 1 va ir iterando

espacio_scrapping <- read_html(espacio)
tds <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td")
nombre_espacio <- html_text(html_nodes(tds, "h1"))
dirección <- html_text(html_nodes(tds, "span[property='v:locality']"))
region <- html_text(html_nodes(tds, "span[property=')v:region']"))
pais <- html_text(html_nodes(tds, "span[property=')v:country']"))
telefono <- html_text(html_nodes(tds, "span[property=')v:tel']"))
                                    
                      
google <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td a")
google_link <- html_attr(google,"href")                      
google_original <- read_html(google_link)



fx_espacios <- function(web,href_espacios,iteracion_esp) {

  link <- paste0(web,hrefs_espacios[1])# el 1iteracion_esp
  espacio_scrapping <- read_html(espacio)
  tds <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td")
  nombre <- html_text(html_nodes(tds, "h1"))
  direccion <- html_text(html_nodes(tds, "span[property='v:street-address']"))
  localidad <- html_text(html_nodes(tds, "span[property='v:locality']"))
  provincia <- html_text(html_nodes(tds, "span[property='v:region']"))
  pais <- html_text(html_nodes(tds, "span[property='v:country']"))
  telefono <- html_text(html_nodes(tds, "span[property='v:tel']"))
  google <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td a")
  google_link <- html_attr(google,"href")      
  coordenadas <- ""# por ahora esta vacio
  web_oficial <- ""#por ahora vacio
  #google_original <- read_html(google_link)
  
  df_provisorio <-  data.frame(
    link,
    nombre, 
    direccion, 
    localidad, 
    provincia,
    pais,
    telefono,
    google_link,a
    coordenadas,
    web_oficial
  )
  df_provisorio <- as.data.frame(df_provisorio)
  df <- rbind(df, nuevo_registro)
  
  return(resultado)
}

# Lista de variables
lista_variables <- list(
  link,
  nombre, 
  direccion, 
  localidad, 
  provincia,
  pais,
  telefono,
  google_link,
  coordenadas,
  web_oficial
)

# Bucle para imprimir el nombre, clase y longitud de cada elemento
for (i in seq_along(lista_variables)) {
  nombre_variable <- names(lista_variables)[i]
  clase <- class(lista_variables[[i]])
  longitud <- length(lista_variables[[i]])
  cat("Nombre:", nombre_variable, "\n")
  cat("Clase:", clase, "\n")
  cat("Longitud:", longitud, "\n")
  cat("\n")
}

