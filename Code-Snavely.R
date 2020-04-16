# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggmap)
#> Loading required package: ggplot2
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = "AIzaSyAAFM75kpNklj1RJrsAyGWDl4lYjD3Rn5g")

install.packages("xlsx")
library(tidyverse)
library(ggplot2)
library(ggmap)
library(xlsx)
setwd("C:/users/sisisnavely/Desktop/GitHub/Sys2202-final-transit/")
cvillemap <- get_map(location = c(-78.4800, 38.0450), maptype = "roadmap", 
                     source = "google", zoom = 13, color="bw")
dfroute1=read.xlsx("routes.xlsx",sheetIndex=1)
dfroute2=read.xlsx("routes.xlsx",sheetIndex=2)
dfroute3=read.xlsx("routes.xlsx",sheetIndex=3)
dfroute4=read.xlsx("routes.xlsx",sheetIndex=4)
dfroute5=read.xlsx("routes.xlsx",sheetIndex=5)
dfroute6=read.xlsx("routes.xlsx",sheetIndex=6)
dfroute7=read.xlsx("routes.xlsx",sheetIndex=7)
dfroute8=read.xlsx("routes.xlsx",sheetIndex=8)
dfroute9=read.xlsx("routes.xlsx",sheetIndex=9)
dfroute10=read.xlsx("routes.xlsx",sheetIndex=10)
dfroute11=read.xlsx("routes.xlsx",sheetIndex=11)
dfroute12=read.xlsx("routes.xlsx",sheetIndex=12)
dfroute13=read.xlsx("routes.xlsx",sheetIndex=13)
df1 <- filter(dfroute1,route == 1 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df2 <- filter(dfroute2,route == 2) %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df3 <- filter(dfroute3,route == 3 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df4 <- filter(dfroute4,route == 4 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df5 <- filter(dfroute5,route == 5 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df6 <- filter(dfroute6,route == 6 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df7 <- filter(dfroute7,route == 7 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df8 <- filter(dfroute8,route == 8 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df9 <- filter(dfroute9,route == 9 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df10 <- filter(dfroute10,route == 10 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df11 <- filter(dfroute11,route == 11 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df12 <- filter(dfroute12,route == 12 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
dft <- filter(dfroute13,route == 13 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
ggmap(cvillemap) + 
  geom_path(data = df1, color = "red", size = 1, lineend = "round") +
  geom_path(data = df2, color = "blue", size = 2.5, lineend = "round") +
  geom_path(data = df3, color = "green", size = 2.5, lineend = "round") +
  geom_path(data = df4, color = "coral4", size = 2.5, lineend = "round") +
  geom_path(data = df5, color = "firebrick3", size = 2.5, lineend = "round") +
  geom_path(data = df6, color = "gold2", size = 2.5, lineend = "round") +
  geom_path(data = df7, color = "darkorange3", size = 2.5, lineend = "round") +
  geom_path(data = df8, color = "darkolivegreen", size = 2.5, lineend = "round") +
  geom_path(data = df9, color = "turquoise", size = 2.5, lineend = "round") +
  geom_path(data = df10, color = "salmon1", size = 2.5, lineend = "round") +
  geom_path(data = df11, color = "plum4", size = 2.5, lineend = "round") +
  geom_path(data = df12, color = "yellow", size = 1, linetype = 2, lineend = "round") +
  geom_path(data = dft, color = "blueviolet", size = 2.5, lineend = "round")
