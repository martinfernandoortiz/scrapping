library(RSelenium)
library(rJava)
library(seleniumPipes)

library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(robotstxt)
setwd("~/Documentos/Mapa de los estadios")
#check if a bot has permissions to access page(s)
#####

#df1 <- read_csv("~/Github/MapaDeLosEstadios/Scrapping Estadios/escrapeodata1.csv")
df1 <- escrapeodata2
df1 <- df1[,-1]
ini <- Sys.time()
for (x  in 5326:10000) {
  if (is.na(df1[x,1])==FALSE) { #para los casos que no tienen link
    errores <- try(read_html(as.character(df1[x,3])), silent = TRUE)
    if(class(errores)!="try-error"){
      
      src <- read_html(as.character(df1[x,3])) 
      
      #ACA VA EL IF PARA VER SI TIENE VENUE
      noestadio <- str_locate((src %>% html_text("Venue")),"Venue")
      if  (is.na(noestadio[1]) == TRUE) {
        tag[x] <-    src %>% html_nodes("dl") %>% html_text()
        df1[x,4] <- tag[x]
        print(x)
        
      } else {
        
        tag[x] <-    src %>% html_nodes("dl") %>% html_text()
        df1[x,4] <- tag[x]
        src1 <- paste0(df1[x,3],"venue/")
        
        Sys.sleep(.4)
        
        urlMapa <-read_html(src1) 
        google <- as.character(urlMapa %>% html_nodes(".block_venue_map")) 
        
        src <- str_locate(google,'" src=\"')
        google <- str_sub(google,src[2],)
        inicio <- str_locate(google,"center=")
        medio <- str_locate(google,",%20")
        fin <- str_locate(google,"&amp;ke")
        longitudA <- as.double(str_sub(google, inicio[2]+1, inicio[2]+9))
        latitudA <- as.double(str_sub(google, medio[2]+1, medio[2]+9))
        
        df1[x,5] <- latitudA
        df1[x,6] <- longitudA
        
        print(x)
        Sys.sleep(.7)
      }
    } else {print("mocho")}
  }
}

#remDr$close()
#rD$server$stop()
#remDr <- remoteDr()
write_csv(df1, "escrapeodata3.csv")

df1[order(-df1$tag), ]
df1 <- df1 %>% mutate(largo= str_length(tag))
chan <- df1 %>% filter(largo==321)
chan$tag
if (str_locate(chan$tag))
  str_locate(chan$tag, "Founded") #1 7
str_locate(chan$tag, "1954") #15 18
str_locate(chan$tag, "Country") #15 18
str_locate(chan$tag, "Name") #15 18
str_locate(chan$tag, "City") #15 18
str_locate(chan$tag, "Capacity") #15 18
