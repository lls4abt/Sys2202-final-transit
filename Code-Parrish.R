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
  # creating a sidebar that allows someone to pick a route number
  sidebarLayout(
    sidebarPanel(
      # creates a dropdown menu of bus routes
      selectInput(
        inputId = "yAxis", 
        label = "Select the Bus Route", 
        selected = "0",
        choices = c("Free Trolley" = "0", "Route 1" = "1", "Route 2" = "2", "Route 3" = "3", "Route 4" = "4", "Route 5" = "5", "Route 6" = "6", "Route 7" = "7", "Route 8" = "8", "Route 9" = "9", "Route 10" = "10", "Route 11" = "11", "Route 12" = "12"))
    ),
    mainPanel(
      # output is a bar graph
      plotOutput("bar", 
      # creating click and hover features that depict data values
      click = "plot_click",
      hover = "plot_hover"),
      verbatimTextOutput("info")
    )
  ),
  # shows the data table of all the data values from aggregated data table
  DT::dataTableOutput("myTable")
))

server2 <- function(input, output){
  
    reactive_data = reactive({
      # converts selected bus route into a number 
      selected_route = as.numeric(input$yAxis)
      # returns row of the selected bus route (which returns the 12 data values from the selected route)
      return(dataFrame2[dataFrame2$Months==selected_route,])
      
    })
    
    output$bar <- renderPlot({
      # creates a vector of different colors for the bars 
      colors = c('orange','lightblue', 'yellow', 'lightgreen', "pink")
      # brings the reactive (selected) data into focus to create the barplot
      our_data <- reactive_data()
      # creates the bar plot using the columns of the data frame which are the months
      barplot(colSums(our_data[,c("Jan.","Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")]),
              ylab="Average Number of Riders",
              xlab="Month",
              names.arg = c("Jan.","Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
              col = colors)
    })
    # creates the text that shows the numbers on the y-axis, representing the number of riders 
    output$info <- renderText({
      xy_str <- function(e){
        if(is.null(e)) return("NULL\n") # if nothing is clicked or hovered over, say NULL
        paste0("Number of Riders = ", round(e$y, 0), "\n") # otherwise, round the number of riders to the nearest whole number
      }
      paste0(
        "Click: ", xy_str(input$plot_click), # creates the box displaying the number of riders under the graph
        "Hover: ", xy_str(input$plot_hover)
      )
    })
    
    output$myTable = DT::renderDataTable({ # displays the entire data table under the graph
      dataFrame2
    })
}
  

shinyApp(ui = ui2, server = server2) # creates the shiny application


