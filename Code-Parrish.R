library(tidyverse)
library(ggplot2)
library(shiny)
library(readxl)
library(xlsx)
library(readr)
# getting the dataframe from excel csv
dataFrame <- read_csv("C:/Users/student/Documents/SYS2202/R/testAgain.csv")
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
      plotOutput("bar")
    )
  )
))

server2 <- function(input, output){
  
    reactive_data = reactive({
      selected_route = as.numeric(input$yAxis)
      return(dataFrame[dataFrame$Months==selected_route,])
      
    })
    
    output$bar <- renderPlot({
      
      our_data <- reactive_data()
      
      barplot(colSums(our_data[,c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")]),
              ylab="Average Number of Riders",
              xlab="Month",
              names.arg = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
              )
    })
}
  

shinyApp(ui = ui2, server = server2)




