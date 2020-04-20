library(tidyverse)
library(ggplot2)
library(shiny)
library(readxl)
library(xlsx)
library(readr)
library(DT)
# getting the dataframe from excel csv
dataFrame2 <- read_csv("C:/Users/student/Documents/SYS2202/R/RidershipByRoute.csv")
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





