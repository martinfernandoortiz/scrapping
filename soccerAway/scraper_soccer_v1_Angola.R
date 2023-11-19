library(RSelenium)
library(rJava)

library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(robotstxt)
setwd("~/Documentos/Mapa de los estadios")
#check if a bot has permissions to access page(s)

paths_allowed(
  paths = c("https://int.soccerway.com/teams/club-teams/")
) #TODO JOYA


url <- read_html("https://int.soccerway.com/teams/club-teams/")
listaPaises <- url %>%  html_nodes(".row")  %>%   html_text() #SACO LA LISTA DE PAISES A ESCRAPEAR
#NOMBRE DE PAIS
for(x in 1:188){ #LOOP PARA LIMPIAR LOS NOMBRES
  listaPaises[x] <- str_sub(listaPaises[x],28,-20)
}
flag <- url %>%  html_nodes(".row") 

for(x in 1:188){ #LOOP PARA LIMPIAR LOS NOMBRES
  fin <- str_locate(flag[x],"_left")[2]
  flag[x] <- str_sub(flag[x],54,fin)
}

##################################################################
#LINK DE PAIS
listaurl <- url %>% 
  html_nodes(xpath='//*[@id="page_teams_1_block_teams_index_club_teams_2"]/ul') %>% 
  html_nodes("a") %>% html_attr("href")


urlCompleta <- listaurl
#LISTA IDs
idis <- url %>% html_nodes("li")%>% html_attr("data-area_id")
idis <- idis[173:360]

length(listaurl)
for (x in 1:188){
  urlCompleta[x] <- paste0("https://es.soccerway.com", listaurl[x])
}
#############################################################################
#############################################################################
#############################################################################

rD <- rsDriver(browser="firefox") #, port=4546L, verbose=F) #conecto a firefox
remDr <- rD[["client"]]
remDr$navigate("https://int.soccerway.com/teams/club-teams/")#pagina en cuestion


################# SELECCIONO EL PAIS ########################################

webElem <- remDr$findElement(using = 'class name', as.character(flag[[128]]))#SELECCIONO Y ABRO EL PAIS

webElem$clickElement() # CLICKEO EN EL EXPANDIBLE

src = remDr$getPageSource()[[1]] #GUARDO LA URL DESPLEGADA

url2 <- read_html(src) # GUARDO LA URL DESPLEGADA

competitions <- url2 %>% html_nodes(".competitions") # ESTO ES PARA SACAR LOS ID DE LOS CAMPEONATOS
competitionsNombre <-url2 %>% html_nodes(".competitions") %>% html_children() %>% html_text()



prueba <- url2 %>% html_nodes(".competitions") %>% html_children() # EL LARGO DE ESTO ES LA CANTIDAD DE NTH-CHILD
length(prueba)


###############NIVEL 2
angolaIndex <- data.frame(link = character(),
                           equipo=character(),
                           genero=character(),
                           liga=character())

#EL FOR LOOP ME DESPLIEGA TODAS LAS CATEGORIAS Y OTROS
for (x in 1:length(prueba)){
  
contador <- x
#contador <- 1 #prueba manual
#ESTO ES PARA QUE VAYA SELECCIONANDO EL CSS DE CADA UNO DE LOS CAMPEONATOS
webElem <- remDr$findElement(using = 'css', str_replace(".competitions > li:nth-child(XXX) > div:nth-child(1)",
                                                        "XXX",
                                                        as.character(contador)))
webElem$clickElement() # CLICKEO EN EL EXPANDIBLE
Sys.sleep(1)

src1 = remDr$getPageSource()[[1]] #GUARDO LA URL DESPLEGADA

url3 <- read_html(src1) # GUARDO LA URL DESPLEGADA
#APARTIR DE ESTO TENGO QUE SACAR LOS NOMBRES DE LOS EQUIPOS, y los links
teams <- url3%>% html_nodes(".team") #


########################## NIVEL 3
linksEquipos <- list()
nombreEquipos <- list()
genero <- list()

#LOOP DE EQUIPOS y LINKS
for (y in 1:length(teams)){
 
  equipoLoop <- teams[y] #prueba manual
  
  positionLink <- str_locate(as.character(equipoLoop),'a href=') #posicion de inicio de link
  positionEquipo <- str_locate(as.character(equipoLoop),'class="team">') #posicion de equipo
  positionFin <- str_locate(as.character(equipoLoop),'</a>') #posicion de fin
  linksEquipos[y] <-   str_sub(equipoLoop,as.integer(positionLink[2])+2,as.integer(positionEquipo[1])-3)
  nombreEquipos[y] <- str_sub(equipoLoop,positionEquipo[2]+1,positionFin[1]-1)
  genero[y] <- "masc"
  
  #por si esta roto
  if (is.na(nombreEquipos[y])==TRUE) {
    positionLink <- str_locate(as.character(equipoLoop),'<a href="') #posicion de inicio de link
    positionEquipo <- str_locate(as.character(equipoLoop),'">') #posicion de equipo
    positionFin <- str_locate(as.character(equipoLoop),'</a>') #posicion de fin
    linksEquipos[y] <-   str_sub(equipoLoop,as.integer(positionLink[2])+1,as.integer(positionEquipo[1])-3)
    nombreEquipos[y] <- str_sub(equipoLoop,positionEquipo[2]+1,positionFin[1]-1)
    genero[y] <- "fem"
  }
  print(paste(as.character(y), as.character(nombreEquipos[y])))
 
}

webElem <- remDr$findElement(using = 'css', str_replace(".competitions > li:nth-child(XXX) > div:nth-child(1)",
                                                        "XXX",
                                                        as.character(contador)))
webElem$clickElement() # CLICKEO EN EL EXPANDIBLE
Sys.sleep(2)

df<- data.frame(unlist(as.character(
                          linksEquipos)),
                unlist(as.character(
                          nombreEquipos)
                ),
                unlist(as.character(genero))
                )
names(df) <- c("link","equipo","genero") 
nombreliga <- as.character(unlist(competitionsNombre[x]))
df <- df %>% mutate(liga = nombreliga)
df <- df[complete.cases(df),]
assign(paste0('angola',competitionsNombre[x]),df )
angolaIndex <- bind_rows(argeliaIndex,df)
}
angolaIndex <- bind_rows(argeliaNPFL, argeliaOther)

angolaIndex<- angolaIndex[-c(93:102),]
write_csv(angolaIndex, "angolaIndex.csv")
# EL LOOP SE HIZO MAL Y SE MULTIPLICARON EQUIPOS Y TIENE EL NOMBRE DE LA LIGA MAL
angolaIndex <- angolaIndex %>% mutate(estadioNombre = "vacio")
angolaIndex <- angolaIndex %>% mutate(capacidad = "0")
angolaIndex <- angolaIndex %>% mutate(latitud = -99999)
angolaIndex <- angolaIndex %>% mutate(longitud = -99999)
estadioNombre <- list()
capacidad <- list()
longitud <- list()
latitud <- list()
#############################################################################################################
#### HASTA ACA SAQUE LOS INDICES POR PAIS ####
angolaIndex2 <- angolaIndex
  for (x  in 51:62) {
    
  urlEstadio <- paste0("https://int.soccerway.com", angolaIndex[x,1])   # CAMBIAR EL 1 por X
  #urlEstadio <- paste0(url,"venue")
a <- character()
a <- read_html(urlEstadio, options = c("NONET","RECOVER", "NOERROR", "NOBLANKS"))
 
if (!is_empty(a)){
  
  remDr$navigate(urlEstadio)#pagina en cuestion
  ur <- read_html(urlEstadio)
  #ACA VA EL IF PARA VER SI TIENE VENUE
  noestadio <- str_locate((ur %>% html_text("Venue")),"Venue")
  if  (is.na(noestadio[1]) == TRUE) {
    estadioNombre[x] <- "XXXXXXXXXXXXXX"
    capacidad[x] <- "XXXXXXXXXXXXXX"
    latitud[x] <- 999999
    longitud[x] = 99999 
    print(paste0(x, as.character(angolaIndex[x,2])))
    
    angolaIndex2[x,5] <- "999"
    angolaIndex2[x,6] <-  "999"
    angolaIndex2[x,7] <- 9999
    angolaIndex2[x,8] <- 9999
    
  } else {
  
  
  
  
  detallesNombre <- read_html(urlEstadio) %>% html_nodes(".details") %>% html_children() %>% html_text()
  detallesNombre
  estadioNombre[x] <- as.character(detallesNombre[2])
  capacidad[x] <- detallesNombre[6]
  
  webElem <- remDr$findElement(using = 'id', 'page_team_1_block_team_venue_7')
  
  webElem$clickElement() # CLICKEO EN EL EXPANDIBLE
  Sys.sleep(2)
  src = remDr$getPageSource()[[1]] #GUARDO LA URL DESPLEGADA
  
  urlMapa <-read_html(src) 
  google <- as.character(urlMapa %>% html_nodes(".block_venue_map")) 
  
  src <- str_locate(google,'" src=\"')
  google <- str_sub(google,src[2],)
  inicio <- str_locate(google,"center=")
  medio <- str_locate(google,",%20")
  fin <- str_locate(google,"&amp;ke")
  longitudA <- as.double(str_sub(google, inicio[2]+1, inicio[2]+9))
  latitudA <- as.double(str_sub(google, medio[2]+1, medio[2]+9))
  
  angolaIndex2[x,5] <- as.character(detallesNombre[2])
  angolaIndex2[x,6] <- detallesNombre[6]
  angolaIndex2[x,7] <- latitudA
  angolaIndex2[x,8] <- longitudA
  
  print(paste0(x, as.character(angolaIndex[x,2])))
      Sys.sleep(1.5)
  }
}
}

write_csv(angolaIndex2,"angolaIndex2.csv")

rD <- rsDriver(browser="firefox") #, port=4546L, verbose=F) #conecto a firefox
remDr <- rD[["client"]]
#remDr$close()
rD$server$stop()


