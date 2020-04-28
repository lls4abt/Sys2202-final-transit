library(tidyverse)
library(ggplot2)
library(shiny)
library(readxl)
library(xlsx)
library(readr)
library(DT)
# getting the dataframe from excel csv
dataFrame <- read_csv("C:/Users/student/Documents/SYS2202/R/RidershipByMonth.csv")
# ui code for Shiny
ui2 <- shinyUI(fluidPage(
  titlePanel("Bus Average Ridership from 2015 to 2019"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yAxis", 
        label = "Select the Month", 
        selected = "January",
        choices = c("January" = "January", "February" = "February", "March" = "March", "April" = "April", "May" = "May", "June" = "June", "July" = "July", "August" = "August", "September" = "September", "October" = "October", "November" = "November", "December" = "December"))
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
    selected_month = as.character(input$yAxis)
    return(dataFrame[dataFrame$Months==selected_month,])
    
  })
  
  output$myTable = DT::renderDataTable({
    dataFrame
  })
  
  output$bar <- renderPlot({
    
    colors = c('orange','lightblue', 'yellow', 'lightgreen', "pink")
    
    our_data <- reactive_data()
    
    barplot(colSums(our_data[,c("FT","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")]),
            ylab="Average Number of Riders",
            xlab="Route Number",
            names.arg = c("FT", "1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
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
}


shinyApp(ui = ui2, server = server2)