##### Lilleth's Portion ######

# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggmap)
#> Loading required package: ggplot2
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
#paid for key that allows for the access of GoogleMaps in R Studio

register_google(key = "AIzaSyAAFM75kpNklj1RJrsAyGWDl4lYjD3Rn5g")

#packages that need to be installed

install.packages("xlsx")
install.packages("reshape")
install.packages("Rcpp")
install.packages("gifski")
install.packages("gganimate")
install.packages("ggplot2")
install.packages("ggmap")
instal.packages("tidyverse")

#libraries that need to be loaded

library(tidyverse)
library(ggplot2)
library(ggmap)
library(xlsx)
library(reshape)
library(gganimate)

#this is the working directory to the folder where the excel sheet is located
#this will need to be adjusted if ran on a different computer

setwd("/users/sisisnavely/Desktop/GitHub/Sys2202-final-transit/")

#zooming in on a map to show Charlottesville
#particularly the area of Charlottesville where the CAT bus routes run

cvillemap <- get_map(location = c(-78.4800, 38.0450), maptype = "roadmap", 
                     source = "google", zoom = 13, color="bw")

#saving each page of the excel to a data frame corresponding to its route
#each sheet of the excel file has all of the longitude & latitude cordinates that make up the particular route

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

#saving the average rider data per route per month to a data frame

dfriders=read.xlsx("routes.xlsx",sheetIndex=14)

#creating a new data frame based on the rider ship data
#this data frame gives a normalized value to each month of each route
#min-max normalization was used for each route so that the monthly ridership value was between 0 and 1
#these value will be adjusted later on so that they are reasonable route widths

dfr <- filter(dfriders,type == "norm") %>%
  select(route,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
colnames(dfr) <- c("route","01-JAN","02-FEB","03-MAR","04-APR","05-MAY"
                   ,"06-JUN","07-JUL","08-AUG","09-SEP","10-OCT","11-NOV","12-DEC")

#organizes the data frame so there are three columns: the route number, the month, and the normalized ridership score

dfr <- 	melt(dfr,id=(c("route")))
colnames(dfr) <- c("route","month","riders")

#orders the data so all of route 1 data rows come first, then all of route 2, then all of route 3, etc.

dfr <- arrange(dfr,route,month)

#filtering out the data frame for route 1
#only need segment a; segments b and c are just repeats of segment a traveling in a different direction
#separate latitude and longitude by identifying comma as the separator
#final columns selected for this cleaned and processed data frame will be longitude, latitude, sequence, and route
#make sure longitude and latitude columns are numeric and not string

df1 <- filter(dfroute1,route == 1 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))

#merging the mapping data with the ridership data for each route into one dataframe

df1 <- merge(df1,dfr,id="route") %>% arrange(month,route,seq)

####SAME STEPS AS ABOVE BUT FOR ROUTES #2-13, route 13 will become known as route t or the trolley route####

df2 <- filter(dfroute2,route == 2) %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df2 <- merge(df2,dfr,id="route") %>% arrange(month,route,seq)
#
df3 <- filter(dfroute3,route == 3 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df3 <- merge(df3,dfr,id="route") %>% arrange(month,route,seq)
#
df4 <- filter(dfroute4,route == 4 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df4 <- merge(df4,dfr,id="route") %>% arrange(month,route,seq)
#
df5 <- filter(dfroute5,route == 5 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df5 <- merge(df5,dfr,id="route") %>% arrange(month,route,seq)
#
df6 <- filter(dfroute6,route == 6 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df6 <- merge(df6,dfr,id="route") %>% arrange(month,route,seq)
#
df7 <- filter(dfroute7,route == 7 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df7 <- merge(df7,dfr,id="route") %>% arrange(month,route,seq)
#
df8 <- filter(dfroute8,route == 8 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df8 <- merge(df8,dfr,id="route") %>% arrange(month,route,seq)
#
df9 <- filter(dfroute9,route == 9 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df9 <- merge(df9,dfr,id="route") %>% arrange(month,route,seq)
#
df10 <- filter(dfroute10,route == 10 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df10 <- merge(df10,dfr,id="route") %>% arrange(month,route,seq)
#
df11 <- filter(dfroute11,route == 11 & seg == "b") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df11 <- merge(df11,dfr,id="route") %>% arrange(month,route,seq)
#
df12 <- filter(dfroute12,route == 12 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
df12 <- merge(df12,dfr,id="route") %>% arrange(month,route,seq)
#
dft <- filter(dfroute13,route == 13 & seg == "a") %>%
  mutate(locclean=str_remove_all(loc,"[^[A-Za-z0-9.,-]]")) %>%
  separate(locclean,c("lon","lat"),sep=",") %>% select(lon,lat,seq,route) %>%
  mutate(lon=as.numeric(lon),lat=as.numeric(lat))
dft <- merge(dft,dfr,id="route") %>% arrange(month,route,seq)

#### ANIMATION MAP FOUR ALL 13 ROUTES ####
#mapping the collection of location points of each route into a path/route
#giving each route a different color
#normalizing the ridership value so that the values are between 0.6 and 3.6 (reasonable route widths)
#route 12 is assigned linetype 2 (a dotted line) so it can be seen since it overlaps other routes
#then animating the map by telling it to transition through the 12 months
#also including a title that displays the current mont of the animation

animap <- ggmap(cvillemap) + 
  geom_path(data = df1, color = "red", size = df1$riders * 3 + .6, lineend = "round") +
  geom_path(data = df2, color = "blue", size = df2$riders * 3 + .6, lineend = "round") +
  geom_path(data = df3, color = "green", size = df3$riders * 3 + .6, lineend = "round") +
  geom_path(data = df4, color = "coral4", size = df4$riders * 3 + .6, lineend = "round") +
  geom_path(data = df5, color = "firebrick3", size = df5$riders * 3 + .6, lineend = "round") +
  geom_path(data = df6, color = "gold2", size = df6$riders * 3 + .6, lineend = "round") +
  geom_path(data = df7, color = "darkorange3", size = df7$riders * 3 + .6, lineend = "round") +
  geom_path(data = df8, color = "darkolivegreen", size = df8$riders * 3 + .6, lineend = "round") +
  geom_path(data = df9, color = "turquoise", size = df9$riders * 3 + .6, lineend = "round") +
  geom_path(data = df10, color = "salmon1", size = df10$riders * 3 + .6, lineend = "round") +
  geom_path(data = df11, color = "plum4", size = df11$riders * 3 + .6, lineend = "round") +
  geom_path(data = df12, color = "yellow", size = df12$riders * 3 + .6, linetype = 2, lineend = "round") +
  geom_path(data = dft, color = "blueviolet", size = dft$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

####the following 13 blocks of code use the same steps as above####
####instead of mapping all 13 routes at the same time it creates a separate animation for each route####
###this will make visualizing the change in ridership for each route a little easier####

#### ANIMATION MAP FOR ROUTE 1 ####

animapr1 <- ggmap(cvillemap) + 
  geom_path(data = df1, color = "red", size = df1$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)
#### ANIMATION MAP FOR ROUTE 2 ####

animapr2 <- ggmap(cvillemap) + 
  geom_path(data = df2, color = "blue", size = df2$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 3 ####

animapr3 <- ggmap(cvillemap) + 
  geom_path(data = df3, color = "green", size = df3$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 4 ####

animapr4 <- ggmap(cvillemap) + 
  geom_path(data = df4, color = "coral4", size = df4$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 5 ####

animapr5 <- ggmap(cvillemap) + 
  geom_path(data = df5, color = "firebrick3", size = df5$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 6 ####

animapr6 <- ggmap(cvillemap) + 
  geom_path(data = df6, color = "gold2", size = df6$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 7 ####

animapr7 <- ggmap(cvillemap) + 
  geom_path(data = df7, color = "darkorange3", size = df7$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 8 ####

animapr8 <- ggmap(cvillemap) + 
  geom_path(data = df8, color = "darkolivegreen", size = df8$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 9 ####

animapr9 <- ggmap(cvillemap) + 
  geom_path(data = df9, color = "turquoise", size = df9$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 10 ####

animapr10 <- ggmap(cvillemap) + 
  geom_path(data = df10, color = "salmon1", size = df10$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 11 ####

animapr11 <- ggmap(cvillemap) + 
  geom_path(data = df11, color = "plum4", size = df11$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE 12 ####

animapr12 <- ggmap(cvillemap) + 
  geom_path(data = df12, color = "yellow", size = df12$riders * 3 + .6, linetype = 2, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### ANIMATION MAP FOR ROUTE TROLLEY ####

animaprt <- ggmap(cvillemap) + 
  geom_path(data = dft, color = "blueviolet", size = dft$riders * 3 + .6, lineend = "round") +
  labs(title="Month: {current_frame}") +
  transition_manual(month)

#### saving all of the animations as gifs

animate(animap, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animap.gif")
animate(animapr1, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr1.gif")
animate(animapr2, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr2.gif")
animate(animapr3, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr3.gif")
animate(animapr4, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr4.gif")
animate(animapr5, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr5.gif")
animate(animapr6, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr6.gif")
animate(animapr7, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr7.gif")
animate(animapr8, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr8.gif")
animate(animapr9, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr9.gif")
animate(animapr10, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr10.gif")
animate(animapr11, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr11.gif")
animate(animapr12, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animapr12.gif")
animate(animaprt, renderer = gifski_renderer(), fps = 1, nframes = 24)
anim_save("animaprt.gif")

#### THE FOLLOWING 12 BLOCK OF CODE SAVE EACH FRAME OF THE TROLLEY ANIMATION AS A PNG ####
#### THE PURPOSE IS TO BE ABLE TO SEE THE RIDERSHIP TRANSITION IN A COLLECTION OF PICTURES AND NOT AS AN ANIMATION ####
#### WAS ONLY DONE WITH TROLLEY ROUTE BECAUSE ONLY TESTING THIS IDEA ####

#
dftmo <- filter(dft,month == "01-JAN")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt01jan.png")
#
dftmo <- filter(dft,month == "02-FEB")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt02feb.png")
#
dftmo <- filter(dft,month == "03-MAR")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt03mar.png")
#
dftmo <- filter(dft,month == "04-APR")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt04apr.png")
#
dftmo <- filter(dft,month == "05-MAY")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt05may.png")
#
dftmo <- filter(dft,month == "06-JUN")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt06jun.png")
#
dftmo <- filter(dft,month == "07-JUL")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt07jul.png")
#
dftmo <- filter(dft,month == "08-AUG")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt08aug.png")
#
dftmo <- filter(dft,month == "09-SEP")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt09sep.png")
#
dftmo <- filter(dft,month == "10-OCT")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt10oct.png")
#
dftmo <- filter(dft,month == "11-NOV")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt11nov.png")
#
dftmo <- filter(dft,month == "12-DEC")
maprtmo <- ggmap(cvillemap) + 
  geom_path(data = dftmo, color = "blueviolet", size = dftmo$riders * 3 + .6) +
  ggtitle(paste("Trolley:",dftmo$month))
ggsave("maprt12dec.png")
#


#### THE FOLLOWING CODE WAS USED TO CREATE AN INTERACTIVE R SHINY APPLICAITON ####
#### THE PROGRAM ALLOWS USERS TO SELECT A ROUTE FROM THE DROP DOWN MENU ####
#### THEY CAN THEN SEE THAT SPECIFIC ROUTE'S RIDERSHIP ANIMATION ####

#install and load RShiny package

install.packages("shiny")
library(shiny)

#creating the ui of the shinyApp
#creating the titles of the possible selections of the drop down menu
#creating a selection for each of the 13 routes
#also creating a selection that will show the 12 months of the trolley data as 12 separate images

ui <- fluidPage(
  fluidRow(column(4,h3("Charlottesville Area Transit Ridership")),
           column(8,  selectInput("route", "Route:", c("All Routes" = "all"
                                                       ,"Route 1" = "r1","Route 2" = "r2","Route 3" = "r3"
                                                       ,"Route 4" = "r4","Route 5" = "r5","Route 6" ="r6"
                                                       ,"Route 7" = "r7","Route 8" = "r8", "Route 9" = "r9"
                                                       ,"Route 10" = "r10", "Route 11" = "r11", "Route 12" = "r12"
                                                       ,"Trolley" = "rt", "Trolley Monthly" = "rtmo")))
  ),
  mainPanel(
    conditionalPanel(
      condition="input.route!='rtmo'",
      imageOutput("outRoute")
    ),
    conditionalPanel(
      condition = "input.route=='rtmo'",
      fluidRow(column(4,div(style="height:50px"),imageOutput("outm01")),
               column(4,div(style="height:50px"),imageOutput("outm02")),
               column(4,div(style="height:50px"),imageOutput("outm03")) ),
      fluidRow(column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm04")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm05")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm06")) ),
      fluidRow(column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm07")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm08")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm09")) ),
      fluidRow(column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm10")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm11")),
               column(4,div(style="font-size:10px; padding: 0px 0px; margin-top:-25em"),imageOutput("outm12")) )
    )
  )
)


#creating the server of the ShinyApp
#this displays the specific gif that correlates to the possible selections of the drop down menu

server <- function(input, output, session) {
  output$outRoute <- renderImage(
    {list(src=if(input$route=="all") {"animap.gif"}
          else {paste0("animap",input$route,".gif")},
          contentType = 'image/gif',width = 800,height = 600)},deleteFile = FALSE)
  
  #this section of code is for the final selection option
  #this selection option displays the 12 months of the trolley data as 12 separate images
  
  output$outm01 <- renderImage({list(src="maprt01jan.png",contentType = 'image/gif',
                                     width = 200,height = 150)},deleteFile = FALSE)
  output$outm02 <- renderImage({list(src="maprt02feb.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm03 <- renderImage({list(src="maprt03mar.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm04 <- renderImage({list(src="maprt04apr.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm05 <- renderImage({list(src="maprt05may.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm06 <- renderImage({list(src="maprt06jun.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm07 <- renderImage({list(src="maprt07jul.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm08 <- renderImage({list(src="maprt08aug.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm09 <- renderImage({list(src="maprt09sep.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm10 <- renderImage({list(src="maprt10oct.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm11 <- renderImage({list(src="maprt11nov.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
  output$outm12 <- renderImage({list(src="maprt12dec.png",contentType = 'image/gif'
                                     ,width = 200,height = 150)},deleteFile = FALSE)
}

#running the shinyApp
shinyApp(ui, server)

###### Andrea's Portion ######
install.packages("DT")

library(tidyverse)
library(readxl)
library(readr)
library(DT)
# getting the dataframe from excel csv
dataFrame2 <- read_csv("/users/sisisnavely/Desktop/GitHub/Sys2202-final-transit/RidershipByRoute.csv")
# ui code for Shiny
ui2 <- shinyUI(fluidPage(
  titlePanel("Bus Average Ridership from 2015 to 2019"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yAxis", 
        label = "Select the Bus Route", 
        selected = "0",
        choices = c("Free Trolley" = "0", "Route 1" = "1", "Route 2" = "2", "Route 3" = "3", "Route 4" = "4", "Route 5" = "5", "Route 6" = "6", "Route 7" = "7", "Route 8" = "8", "Route 9" = "9", "Route 10" = "10", "Route 11" = "11", "Route 12" = "12"))
    ),
    mainPanel(
      plotOutput("bar", 
                 click = "plot_click",
                 hover = "plot_hover"),
      verbatimTextOutput("info")
    )
  ),
  DT::dataTableOutput("myTable")
))

server2 <- function(input, output){
  
  reactive_data = reactive({
    selected_route = as.numeric(input$yAxis)
    return(dataFrame2[dataFrame2$Months==selected_route,])
    
  })
  
  output$bar <- renderPlot({
    
    colors = c('orange','lightblue', 'yellow', 'lightgreen', "pink")
    
    our_data <- reactive_data()
    
    barplot(colSums(our_data[,c("Jan.","Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")]),
            ylab="Average Number of Riders",
            xlab="Month",
            names.arg = c("Jan.","Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
            col = colors)
  })
  
  output$info <- renderText({
    xy_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0("Number of Riders = ", round(e$y, 0), "\n")
    }
    paste0(
      "Click: ", xy_str(input$plot_click),
      "Hover: ", xy_str(input$plot_hover)
    )
  })
  
  output$myTable = DT::renderDataTable({
    dataFrame2
  })
}


shinyApp(ui = ui2, server = server2)

###### Shivani's Porion ######

install.packages("xlsx")
library(tidyverse)
library(ggplot2)
library(readxl)
library(xlsx)
library(readr)

# getting the dataframe from the csv
df1 <- read.csv("C:/Users/shiva/Desktop/SYS 2202/Bus Rider Normalization1.csv")


# Stacked barplot with multiple groups
ggplot(data=df1, aes(x=Months, y=Needed, fill=Routes)) +
  geom_bar(stat="identity")  

# Stacked barplot with multiple groups
ggplot(data=df1, aes(x=Months, y=Used, fill=Routes)) +
  geom_bar(stat="identity")


###### Claire's Portion ######








