
######################### ESTADIOS.ORG #######################################3


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
  paths = c("http://www.estadios.org/futbol/1/"))
#TODO JOYA





########################## INICIO ####################################
#Creo Data Frame vacio

tablafinal <- data.frame(pais=character(),
                         nombre=character(), 
                         provincia=character(),
                         ciudad=character(),
                         equipo=character(),
                         deporte=character(),
                         ano=character(),
                         capacidad=character(),
                         enlace=character(),
                         enlaceIndice=character()
                                              )


#ACA SETEO CUANTAS PAGINAS QUIERO ESCRAPEAR, EN ESTE CASO ES LA 1 y LA 2. LA VARIABLE CON EL NUMERO SE LLAMA WEB
      
      for (web in 53:55){
        
        #Sys.sleep(1)
        
        indice <- as.character(
          paste("http://www.estadios.org/futbol/",web,"/",sep='') #paste("http://www.estadios.org/futbol/",web,"/",sep='')
        )
        
        pg_indice <- read_html(
          as.character(
            paste("http://www.estadios.org/futbol/",web,"/",sep='')#  paste("http://www.estadios.org/futbol/",web,"/",sep='')
          )
        )
        
        
        #Extraigo los links de cada cancha segun el número e página 
        listado <- (html_attr(html_nodes(pg_indice, "a"), "href"))
        listado <- (listado[11:59])
        listado
        
        #Creo un contador en base al número máximo de canchas que tenga esa página
        contadorMax <- length(listado)
        
        #creo el loop
        for (x in 1:contadorMax){  #1:contadorMax
          
          Sys.sleep(5)
          
          
          # Creo la variable con el link que contiene la ficha del estadio
          
          #URL <- "https://www.estadiosdeargentina.com.ar/cancha-ferro-caballito/"
          URL <- paste("http://www.estadios.org/", listado[x],sep='') #cambiar el 1 por x
          # Leo el html de ese link/variable
          #URL <- as.character(URL)
          #pg <- (read_html(URL))
          
          # Extraigo la info
          title_data_html <- read_html(URL) %>% html_nodes(xpath = "//body//table//tr//td//table//tr//td//table//tr//td//table//tr//td" ) #%>% 
          #   html_nodes("td")
          title_data_html <- title_data_html[1:8]
          #Converting the title data to text
          title_data <- html_text(title_data_html)
          title_data <- iconv(title_data,"WINDOWS-1252","UTF-8")
          
          ############ EXTRACCIÓN DE VARIABLES ##################
          #substr(x, start, stop) considerar el ifelse en el start por si hay diferentes formatos
          
          #primer campo a extraer pais
          title_data
          if(substr(title_data[3],1,3) =="Pro") #La razon del if, es que la información en la pagina, esta presentada en diferentes formatos, a veces dice provincia y otras no. Todo lo que esta en el else era el formato original
          {pais <- substr(title_data[1], #el vector
                          ifelse(
                            substr(title_data[1],1,6)== "País: " ,
                            7,7
                          ), #el inicio de la extracción
                          stringr::str_length(title_data[1])
          ) 
          
          pais <- iconv(pais,"UTF-8","WINDOWS-1252")
          #segundo campo a extraer nombre
          nombre <- substr(title_data[2], #el vector
                           ifelse(
                             substr(title_data[2],1,8)== "Nombre: " ,
                             9,9
                           ), #el inicio de la extracción
                           stringr::str_length(title_data[2])
          )
          nombre <- iconv(nombre,"UTF-8","WINDOWS-1252")
          
          # campo a extraer provincia
          provincia <- substr(title_data[3], #el vector
                              ifelse(
                                substr(title_data[3],1,18) == "Provincia/Estado: " ,
                                19,18
                              ), #el inicio de la extracción
                              stringr::str_length(title_data[3])
          )
          
          provincia <- iconv(provincia,"UTF-8","WINDOWS-1252")
          
          #extraer ciudad
          ciudad <- substr(title_data[4], #el vector
                           ifelse(
                             substr(title_data[4],1,8) == "Ciudad: " ,
                             9,9
                           ), #el inicio de la extracción
                           stringr::str_length(title_data[4])
          )
          ciudad <- iconv(ciudad,"UTF-8","WINDOWS-1252")
          
          #quinto campo a extraer Equipo/Uso: 
          equipo <- substr(title_data[5], #el vector
                           ifelse(
                             substr(title_data[5],1,12)== "Equipo/Uso: " ,
                             13,13
                           ), #el inicio de la extracción
                           stringr::str_length(title_data[5])
          )
          equipo <- iconv(equipo,"UTF-8","WINDOWS-1252")
          #quinto campo a extraer Deporte
          deporte <- substr(title_data[6], #el vector
                            ifelse(
                              substr(title_data[6],1,8)== "Deporte:" ,
                              10,9
                            ), #el inicio de la extracción
                            stringr::str_length(title_data[6])
          )
          deporte <- iconv(deporte,"LATIN1","UTF-8")
          #sexto campo a extraer ano
          ano <- substr(title_data[7], #el vector
                        ifelse(
                          substr(title_data[7],1,21)== "Año de construcción:" ,
                          21,22
                        ), #el inicio de la extracción
                        stringr::str_length(title_data[7])
          )
          ano <- iconv(ano,"UTF-8","WINDOWS-1252")
          #séptimo campo a extraer Capacidad
          capacidad <- substr(title_data[8], #el vector
                              ifelse(
                                substr(title_data[8],1,11)== "Capacidad: " ,
                                12,13
                              ), #el inicio de la extracción
                              stringr::str_length(title_data[8])
          )
          capacidad <- iconv(capacidad,"UTF-8","WINDOWS-1252")
          } else  { # Termina el if
            
            
            
            pais <- substr(title_data[1], #el vector
                           ifelse(
                             substr(title_data[1],1,6)== "País: " ,
                             7,7
                           ), #el inicio de la extracción
                           stringr::str_length(title_data[1])
            ) 
            
            pais <- iconv(pais,"UTF-8","WINDOWS-1252")
            
            #segundo campo a extraer nombre
            nombre <- substr(title_data[2], #el vector
                             ifelse(
                               substr(title_data[2],1,8)== "Nombre: " ,
                               9,9
                             ), #el inicio de la extracción
                             stringr::str_length(title_data[2])
            )
            nombre <- iconv(nombre,"UTF-8","WINDOWS-1252")
            provincia <- "vacio"
            #tercer campo a extraer ciudad
            ciudad <- substr(title_data[3], #el vector
                             ifelse(
                               substr(title_data[3],1,8) == "Ciudad: " ,
                               9,9
                             ), #el inicio de la extracción
                             stringr::str_length(title_data[3])
            )
            
            ciudad <- iconv(ciudad,"UTF-8","WINDOWS-1252")
            #cuarto campo a extraer Equipo/Uso: 
            equipo <- substr(title_data[4], #el vector
                             ifelse(
                               substr(title_data[4],1,12)== "Equipo/Uso: " ,
                               13,13
                             ), #el inicio de la extracción
                             stringr::str_length(title_data[4])
            )
            
            equipo <- iconv(equipo,"UTF-8","WINDOWS-1252")
            #quinto campo a extraer Deporte
            deporte <- substr(title_data[5], #el vector
                              ifelse(
                                substr(title_data[5],1,8)== "Deporte:" ,
                                10,9
                              ), #el inicio de la extracción
                              stringr::str_length(title_data[5])
            )
            
            deporte <- iconv(deporte,"LATIN1","UTF-8")
            #sexto campo a extraer ano
            ano <- substr(title_data[6], #el vector
                          ifelse(
                            substr(title_data[6],1,21)== "Año de construcción:" ,
                            21,22
                          ), #el inicio de la extracción
                          stringr::str_length(title_data[6])
            )
            ano <- iconv(ano,"UTF-8","WINDOWS-1252")
            #séptimo campo a extraer Capacidad
            capacidad <- substr(title_data[7], #el vector
                                ifelse(
                                  substr(title_data[7],1,11)== "Capacidad: " ,
                                  12,13
                                ), #el inicio de la extracción
                                stringr::str_length(title_data[7])
            )
            capacidad <- iconv(capacidad,"UTF-8","WINDOWS-1252")
          }
          
          # Para saber de que página índice estamos hablando
          numeroweb <- web
          # Para saber el enlace donde esta la web
          enlace <- URL
          # Para saber el enlace Indice donde esta la web
          enlaceIndice <- indice
          
          
          # Creo el dataframe
          tabla <- data.frame(pais, nombre, provincia, ciudad, equipo,deporte, 
                              ano, capacidad, enlace, enlaceIndice)
          
          
          #Lo uno al resto de los dataframe
          tablafinal <- tablafinal %>% dplyr::bind_rows(tabla)
          #Termina el loop de carga
        } #Termina el loop de estadio
      } # Termina el loop de pagina

write.csv(tablafinal, "consolidado_estadiosorg1-93.csv")

# Limpie los acentos desde excel
tablafinal1 <- read_csv("consolidado_estadiosorg1-93.csv")



tablafinal1 <- tablafinal1 %>% mutate(pais= str_sub(pais,1,str_length(pais)-3))
tablafinal1 <- tablafinal1 %>% mutate(ano=str_replace(ano,": -",""))
tablafinal1 <- tablafinal1 %>% mutate(ano=str_replace(ano,": ",""))
tablafinal1 <- tablafinal1 %>% mutate(capacidad=str_replace(capacidad," ",""))
write.csv(tablafinal, "consolidado_estadiosorg1-93_v1.csv")

# Geocode
library(tidygeocoder)

stadium_locations <- tablafinal1 %>%
  geocode(street = nombre, city = ciudad, country = pais, method = 'osm', 
          full_results = TRUE, custom_query= list(extratags = 1))

stadium_locations[1,1:17]
write.csv(stadium_locations[,1:17], "consolidado_estadiosorg1-93_v1_coords.csv")


prueba <- tablafinal1 %>% filter(pais== "Alemania")# %>% group_by(nombre) %>% count()

count(stadium_locations$lat)

a1<- stadium_locations %>% group_by(lat) %>% count()

paises <- tablafinal1 %>% group_by(pais) %>% count()

  #########################################################################################

###################### THE END ##########################################################

##########################################################################################

  