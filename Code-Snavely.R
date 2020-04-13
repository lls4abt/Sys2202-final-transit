# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggmap)
#> Loading required package: ggplot2
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = "AIzaSyAAFM75kpNklj1RJrsAyGWDl4lYjD3Rn5g")

# check if key is saved
has_goog_key()
#> [1] TRUE

install.packages("xlsx")
library(tidyverse)
library(ggplot2)
library(ggmap)
library(xlsx)
setwd("/users/sisisnavely/Dektop/GitHub/Sys2202-final-transit/")
cvillemap <- get_map(location = c(-78.4800, 38.0450), maptype = "roadmap", 
                     source = "google", zoom = 13, color="bw")
dfroute1=read.xlsx("route1.xlsx",sheetIndex=1)
dfroute2=read.xlsx("route2.xlsx",sheetIndex=1)
df1a <- filter(dfroute1,route =="1a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df1b <- filter(dfroute1,route =="1b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df1c <- filter(dfroute1,route =="1c") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df2a <- filter(dfroute2,route =="2a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df2b <- filter(dfroute2,route =="2b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
ggmap(cvillemap) + 
  geom_path(data = df1a, color = "red", size = 1, lineend = "round") +
  geom_path(data = df1b, color = "red", size = 1, lineend = "round") +
  geom_path(data = df1c, color = "red", size = 1, lineend = "round") +
  geom_path(data = df2a, color = "blue", size = 2.5, lineend = "round") +
  geom_path(data = df2b, color = "blue", size = 2.5, lineend = "round")

#newcomment