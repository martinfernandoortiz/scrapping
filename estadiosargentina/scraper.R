
######################### ESCRAPEO DE ESTADIOS DE ARGENTINA #######################################3


library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(robotstxt)
setwd("~/Documentos/Mapa de los estadios")
#check if a bot has permissions to access page(s)

paths_allowed(
  paths = c("https://www.estadiosdeargentina.com.ar/category/estadios-abiertos-futbol/page/2/")
) #TODO JOYA



#check if a bot has permissions to access page(s)

paths_allowed(
  paths = c("https://www.estadiosdeargentina.com.ar/category/estadios-abiertos-futbol/page/2/")
) #TODO JOYA

#SON 111 paginas que hay que sacar los links de cada pagina



########################## INICIO ####################################
#Creo Data Frame vacio

tablafinal <- data.frame(propietario=character(),
                         fundacion=character(), 
                         afiliacion=character(),
                         nombreOficial=character(),
                         inauguracion=character(),
                         direccion=character(),
                         capacidad=character(),
                         numeroweb=integer(),
                         enlace=character(),
                         enlaceIndice=character()
                        )



#ACA SETEO CUANTAS PAGINAS QUIERO ESCRAPEAR, EN ESTE CASO ES LA 1 y LA 2. LA VARIABLE CON EL NUMERO SE LLAMA WEB
for (web in 2:101){
  
  indice <- as.character(
    paste("https://www.estadiosdeargentina.com.ar/category/estadios-abiertos-futbol/page/",web,"/",sep='')
  )
  
  pg_indice <- read_html(
    as.character(
      paste("https://www.estadiosdeargentina.com.ar/category/estadios-abiertos-futbol/page/",web,"/",sep='')
    )
  )







#Extraigo los links de cada cancha segun el número e página 
listado <- (html_attr(html_nodes(pg_indice, ".read-more"), "href"))

#Creo un contador en base al número máximo de canchas que tenga esa página
contadorMax <- length(listado)

#creo el loop
for (x in 1:contadorMax){
 



# Creo la variable con el link que contiene la ficha del estadio

#URL <- "https://www.estadiosdeargentina.com.ar/cancha-ferro-caballito/"
URL <- listado[x]
# Leo el html de ese link/variable

pg <- read_html(URL)

# Extraigo la info
title_data_html <- 
  pg %>% html_nodes("tr") #%>% html_nodes("left")

#Converting the title data to text
title_data <- html_text(title_data_html)


  ############ EXTRACCIÓN DE VARIABLES ##################
  #substr(x, start, stop) considerar el ifelse en el start por si hay diferentes formatos

    
  #primer campo a extraer Propietario
  propietario <- substr(title_data[1], #el vector
                        ifelse(
                          substr(title_data[1],1,23)== "Propietario del estadio" ,
                          24,12
                        ), #el inicio de la extracción
                        stringr::str_length(title_data[1])
  )
  
  #segundo campo a extraer Fecha Fundación
  fundacion <- substr(title_data[2], #el vector
                        ifelse(
                          substr(title_data[2],1,31)== "Fecha de la fundación del club" ,
                                  31,28
                        ), #el inicio de la extracción
                        stringr::str_length(title_data[2])
  )
  
  #tercer campo a extraer Afiliación
  afiliacion <- substr(title_data[3], #el vector
                      ifelse(
                        substr(title_data[3],1,27) == "Afiliación / Liga de origen" ,
                        28,29
                      ), #el inicio de la extracción
                      stringr::str_length(title_data[3])
  )
  
  
  #cuarto campo a extraer Nombre Oficial
  nombreOficial <- substr(title_data[4], #el vector
                       ifelse(
                         substr(title_data[4],1,14)== "Nombre oficial" ,
                         15,16
                       ), #el inicio de la extracción
                       stringr::str_length(title_data[4])
  )
  
  #quinto campo a extraer Inauguración
  inauguracion <- substr(title_data[5], #el vector
                          ifelse(
                            substr(title_data[5],1,21)== "Fecha de inauguración" ,
                            22,23
                          ), #el inicio de la extracción
                          stringr::str_length(title_data[5])
  )
  
  #sexto campo a extraer Dirección
  direccion <- substr(title_data[6], #el vector
                          ifelse(
                            substr(title_data[6],1,9)== "Dirección" ,
                            10,11
                          ), #el inicio de la extracción
                          stringr::str_length(title_data[6])
  )
  
  #séptimo campo a extraer Capacidad
  capacidad <- substr(title_data[7], #el vector
                          ifelse(
                            substr(title_data[7],1,9)== "Capacidad" ,
                            10,11
                          ), #el inicio de la extracción
                          stringr::str_length(title_data[7])
  )
  
  # Para saber de que página índice estamos hablando
numeroweb <- web

  # Para saber el enlace donde esta la web
  enlace <- URL
  # Para saber el enlace Indice donde esta la web
enlaceIndice <- indice
  
  
  # Creo el dataframe
tabla <- data.frame(propietario, fundacion, afiliacion, nombreOficial, 
                    inauguracion, direccion, capacidad, numeroweb, enlace, enlaceIndice)


 #Lo uno al resto de los dataframe
tablafinal <- tablafinal %>% dplyr::bind_rows(tabla)

}
}

write.csv(tablafinal, "consolidado.csv")
#########################################################################################

###################### THE END ##########################################################

##########################################################################################

####### OTRO FORMATO!

## Aca voy a seleccionar todos los que tienen el otro formato para luegos unirlos y filtrar solo los na
# Cargo tablas
library(readr)
estadiosArgentina2Y3 <- read_csv("estadiosArgentina2Y3.csv",
                                 col_types = cols(X1 = col_skip()))
estadiosArgentina4Y10 <- read_csv("estadiosArgentina4Y10.csv",
                                  col_types = cols(X1 = col_skip()))
estadiosArgentina11Y20 <- read_csv("estadiosArgentina11Y20.csv",
                                   col_types = cols(X1 = col_skip()))
estadiosArgentina21Y40 <- read_csv("estadiosArgentina21Y40.csv",
                                   col_types = cols(X1 = col_skip()))
estadiosArgentina41Y80 <- read_csv("estadiosArgentina41Y80.csv",
                                   col_types = cols(X1 = col_skip()))
estadiosArgentina81Y101 <- read_csv("estadiosArgentina81Y111.csv",
                                    col_types = cols(X1 = col_skip()))

## Hago la unión
tablaGeneral <- dplyr::bind_rows(estadiosArgentina2Y3,estadiosArgentina4Y10,estadiosArgentina11Y20,
                                 estadiosArgentina21Y40,estadiosArgentina41Y80,estadiosArgentina81Y101)
# Selecciono campos
tablaGeneral <-  tablaGeneral %>% select (propietario,numeroweb,enlace,enlaceIndice)

#Mando filtro
 tablaGeneralFiltrada <-  filter(tablaGeneral, is.na.data.frame(propietario))


listadoDos <- as.vector(tablaGeneralFiltrada$enlace)
contador2 <- length(listadoDos)

################################ DATA FRAME VACIO ###############################################

tablaFormatofinal <- data.frame(equipo=character(),
                         nombre=character(), 
                         inauguracion=character(),
                         direccion=character(),
                         capacidad=character(),
                         numeroweb=character(),
                         enlace=character()
                          )




############################## MODELO DE FORMATO 2 ##########################################################
# Loop
 for (x in 1:contador2){

# Creo la variable con el link que contiene la ficha del estadio

#URL <- "https://www.estadiosdeargentina.com.ar/cancha-atletico-9-de-julio-buenos-aires/"
URL <- listadoDos[x]
# Leo el html de ese link/variable

pg <- read_html(URL)

# Extraigo la info
title_data_html <- 
  pg %>% html_nodes("div")
title_data_html <- title_data_html[16:20]
#Converting the title data to text
title_data <- html_text(title_data_html)


equipo <- substr(title_data[1], 
                 str_length(" Equipo: ")+1,
                 str_length(title_data[1])
                )


nombre <- substr(title_data[2], 
                 str_length(" Nombre del estadio: ")+1,
                 str_length(title_data[2])
                   )


inauguracion <- substr(title_data[3], 
                 str_length(" Fecha de inauguración: ")+1,
                 str_length(title_data[3])
                      )


direccion <- substr(title_data[4], 
                    str_length(" Dirección: ")+1,
                    str_length(title_data[4])
                    )



capacidad <- substr(title_data[5], 
                    str_length(" Capacidad: ")+1,
                    str_length(title_data[5])
                    )


# Para saber el enlace donde esta la web
enlace <- URL



# Creo el dataframe
tablaFormato <- data.frame(equipo, nombre, inauguracion, direccion, 
                    capacidad, enlace)


#Lo uno al resto de los dataframe
tablaFormatofinal <- tablaFormatofinal %>% dplyr::bind_rows(tablaFormato)


 }
write_csv(tablaFormatofinal,"otroformato.csv")



############################# Y LAS COORDENADAS ##############################################


URL <- "https://www.estadiosdeargentina.com.ar/cancha-atletico-9-de-julio-buenos-aires/"
#URL <- listadoDos[x]
# Leo el html de ese link/variable

pg <- read_html(URL)

# Extraigo la info
title_data_html <- 
  pg %>% html_nodes(".address") 
a <- title_data_html[4]

#mapDiv > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3)
.google-maps-links
"bottom-actions"
.address

#Converting the title data to text
title_data <- html_text(title_data_html)
title_data[5]
write(title_data[8],"borrar.csv")
      