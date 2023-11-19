library(rvest)
library(tidyverse)
library(stringr)

linkedin_URL <- function(a,b,c){
  url <- paste(a,b,sep="")
  url <- paste(url,c,sep="")
}

# Linkedin URL
link <- "https://www.linkedin.com/jobs/search?"
keyword <- "keywords=gis&"
location <-  "location=Argentina&"


url <- linkedin_URL(link,keyword,location) #armo la url

linkedin <- read_html(url) #leo el html



cantidadDeTrabajos <- linkedin %>% html_nodes("[class='results-context-header__job-count']") #resumen de cantidad de ofertas
paisDeTrabajos <- linkedin %>% html_nodes("[class='results-context-header__query-search']") %>% html_text() #resumen



a <- data.frame(
  ofertasTitulo = linkedin %>% html_elements('.base-search-card__title') %>% html_text2(),
  ofertasEmpresa = linkedin %>% html_elements(".base-search-card__subtitle") %>% html_text2(),
  ofertasLocation = linkedin %>% html_elements(".job-search-card__location") %>% html_text2(),
  ofertaSDate = linkedin %>% html_nodes(xpath = "//time") %>% html_attr("datetime"),
  ofertasLink = linkedin %>% html_elements("[class='base-card__full-link absolute top-0 right-0 bottom-0 left-0 p-0 z-[2]']") %>% 
    html_attr("href")
)

a[ , 'texto'] <- NA

for (x in 1:length(a$ofertasLink)){
  print(x)
  url <- a$ofertasLink[1] #armo la url
  trabajo <- read_html(url)
  texto1 <- trabajo %>% html_elements("[section='description__text description__text--rich']") %>% html_text2()
  a$texto[x] <- texto
 # print(trabajo)
}
··············

library(RSelenium)
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open()

remDr$navigate("http://www.google.com/ncr")
remDr$getStatus()

remDr$navigate("http://www.bbc.co.uk")
remDr$getCurrentUrl()
