library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(robotstxt)

#setwd("~/Documentos/Mapa de los estadios")


paths_allowed(
  paths = c("http://www.alternativateatral.com.ar/teatros.asp")
) #TODO JOYA





######################################################################
########################## INICIO ####################################
######################################################################

######################################################################
########################## VARIABLES #################################
######################################################################

#Variables que se usan todo el tiempo
web <- "http://www.alternativateatral.com.ar/" 
pagina <- read_html("http://www.alternativateatral.com.ar/teatros.asp?pais=1")


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


df_zonal <- data.frame (zona = character(),
                        href = character(), 
                        prov= character(),
                        pagina_zonas=character()
                        )

lista_zonal <- list()
hrefs_espacios <- list()
######################################################################
########################## FUNCIONES #################################
######################################################################


######################################################################
##################### FX ZONAS POR PROVINCIA #########################
######################################################################

# Iteración en provincias para sacar zonas. Cada provincia va a tener muchas zonas
#loops sobre loops

fx_zonas <- function(df_zonal,hrefs_provincia,numero_de_provincia) {
  
  zonas <- paste0(web,hrefs_provincia[numero_de_provincia]) # el 1 va ir iterando
  pagina_provincias <- read_html(zonas)
  lista_li <- html_nodes(pagina_provincias, "#zonas li a") #extraigo los nodos de zona
  hrefs_zonas <- html_attr(lista_li, "href") #hrefs a las zonas
  text_zonas <- html_text(lista_li) #texto de zona
  nombre_provincia <- valores[numero_de_provincia] #numero_de_provincia
  df_zonal_provisorio <- data.frame(zona = text_zonas, href = hrefs_zonas, prov=nombre_provincia) #esto es para ordenar los loops
  df_zonal_provisorio <- df_zonal_provisorio %>% mutate(paginas_zonas= paste0(web,href))
  df_zonal <- rbind(df_zonal, df_zonal_provisorio)
  pagina_zonas <- paste0(web,hrefs_zonas) # el 1 va ir iterando
  lista_zonal <- append(lista_zonal, pagina_zonas)
  return(df_zonal)
}  

######################################################################
##################### FX ESPACIOS POR ZONA ###########################
######################################################################

fx_zonas_espacios <- function(paginas_zonas) {
  
  for (x in 1:length(paginas_zonas)){
    pagina_espacios <- read_html(paginas_zonas[x])
    #tds <- html_nodes(pagina_espacios, "td.celdadatos a") #para ver los links
    
    # Con esto se evita cuando hay mas de un link
    nodo_b <- pagina_espacios %>%
      html_node("td.celdadatos:nth-child(2) > a:nth-child(1)")
    
    # Extraer el atributo href del enlace
    hrefs_espacios_provisorio <- html_attr(nodo_b, "href")
    

    
    
    # Extraer el atributo href de los elementos <td>
    #hrefs_espacios_provisorio <- html_attr(tds, "href")#links a la web... el largo sera la q de 
    hrefs_espacios_provisorio <- paste0(web,hrefs_espacios_provisorio)
    print(x)
    hrefs_espacios_provisorio <- as.list(hrefs_espacios_provisorio)
    hrefs_espacios <- append(hrefs_espacios, hrefs_espacios_provisorio)
  }
  
  return(hrefs_espacios)
}

######################################################################
####################### FX  ESCRAPEO ESPACIOS ########################
######################################################################

#Falta el tema de coordenadas o georef

fx_espacios <- function(df,web,href_espacios,iteracion_esp) {
  
      for (x in 1:iteracion_esp){
        
              link <- as.character(hrefs_espacios[x])# el 1iteracion_esp
              espacio_scrapping <- read_html(link)
              tds <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td")
              nombre <- reemplazar_nulo(html_text(html_nodes(tds, "h1")))
              direccion <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:street-address']")))
              localidad <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:locality']")))
              provincia <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:region']")))
              pais <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:country']")))
              telefono <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:tel']")))

              enlaces <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td a")    
              enlaces_mapa <- enlaces[html_text(enlaces) == "(mapa)"]
              google_link <- reemplazar_nulo(html_attr(enlaces_mapa, "href"))
              
                 
              coordenadas <- "0"
              web_oficial <-reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:url']")))
              #google_original <- read_html(google_link)
              
              df_provisorio <-  data.frame(
                link = link,
                nombre = nombre, 
                direccion = direccion, 
                localidad = localidad, 
                provincia = provincia,
                pais = pais,
                telefono = telefono,
                google_link = google_link,
                coordenadas = coordenadas,
                web_oficial = web_oficial,
                stringsAsFactors = FALSE
              )
              
            df <- rbind(df, df_provisorio)
      }
  return(df)
}

######################################################################
##########################  Es Nulo  #################################
######################################################################
reemplazar_nulo <- function(valor) {
  if (length(valor) == 0) {
    return("NaN")
  } else if (is.na(valor) || valor == "") {
    return("NaN")
  } else {
    return(valor)
  }
}

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################


######################################################################
########################## PROVINCIAS ################################
######################################################################

lista <- html_nodes(pagina, "#provincias li a") #lista de provincias con text y href
valores <- html_text(lista) #nombre de las provincias 
hrefs_provincia <- html_attr(lista, "href") #links a las provincias

df_provincial <- data.frame(valor = valores, href = hrefs_provincia) #esto es para ordenar los loops
loop_1 <-length(valores)

######################################################################
########################## ZONAS #####################################
######################################################################

# Iteración en provincias para sacar zonas. Cada provincia va a tener muchas zonas
#loops sobre loops

for (x in 1:24){
  df_zonal <- fx_zonas(df_zonal,hrefs_provincia,x) 
  
}

paginas_zonas <- c(df_zonal$paginas_zonas)

######################################################################
####################### ZONAS ESPACIOS ###############################
######################################################################

#Extraer data para luego ir a los espacios
  





hrefs_espacios <- fx_zonas_espacios(paginas_zonas)
len_espacios <- length(hrefs_espacios)


######################################################################
########################## ESCRAPEO  #################################
######################################################################

df <- fx_espacios (df,web,hrefs_espacios,len_espacios)

fx_espacios <- function(df,web,href_espacios,iteracion_esp) {
  
  for (x in 1:10){
    
    link <- as.character(hrefs_espacios[x])# el 1iteracion_esp
    espacio_scrapping <- read_html(link)
    tds <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td")
    nombre <- reemplazar_nulo(html_text(html_nodes(tds, "h1")))
    direccion <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:street-address']")))
    localidad <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:locality']")))
    provincia <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:region']")))
    pais <- reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:country']")))
    telefono <- reemplazar_nulo(as.character(html_text(html_nodes(tds, "span[property='v:tel']"))))
    
    enlaces <- html_nodes(espacio_scrapping, "td.ficha-cabecera-td a")    
    enlaces_mapa <- enlaces[html_text(enlaces) == "(mapa)"]
    google_link <- reemplazar_nulo(html_attr(enlaces_mapa, "href"))
    
    coordenadas <- "0"
    web_oficial <-reemplazar_nulo(html_text(html_nodes(tds, "span[property='v:url']")))
    #google_original <- read_html(google_link)
    
    df_provisorio <-  data.frame(
      link = link,
      nombre = nombre, 
      direccion = direccion, 
      localidad = localidad, 
      provincia = provincia,
      pais = pais,
      telefono = telefono,
      google_link = google_link,
      coordenadas = coordenadas,
      web_oficial = web_oficial,
      stringsAsFactors = FALSE
    )
    
    df <- rbind(df, df_provisorio)
    print(x)
  }
  return(df)
  print(x)
}

write.csv(df, "df.txt")

