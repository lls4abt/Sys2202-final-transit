library(tidyverse)
library(ggplot2)
library(shiny) 
library(readxl)
library(xlsx)
library(readr) 
library(DT)

dataFrame2 <- read_excel("C:\\Users\\student\\Documents\\SYS2202\\SYS 2202 Final Project Dataframe.xlsx")

# create new column: total fuel saved 
dataFrame2$TotalFuel      <- 0

# add fuel saved per route columns to calculate total fuel saved
dataFrame2$TotalFuel <- dataFrame2$TotalFuel + dataFrame2$`Free Trolley Fuel Saved (gallons)`+ dataFrame2$`Route 1 Fuel Saved (gallons)`+dataFrame2$`Route 2 Fuel Saved (Gallons)`+dataFrame2$`Route 3 Fuel Saved (Gallons)`+dataFrame2$`Route 5 Fuel Saved (Gallons)...36`+dataFrame2$`Route 5 Fuel Saved (Gallons)...43`+dataFrame2$`Route 7 Fuel Saved (Gallons)...50`+dataFrame2$`Route 7 Fuel Saved (Gallons)...57`+dataFrame2$`Route 8 Fuel Saved (Gallons)`+dataFrame2$`Route 9 Fuel Saved (Gallons)`+dataFrame2$`Route 11 Fuel Saved (Gallons)...78`+dataFrame2$`Route 11 Fuel Saved (Gallons)...85`+dataFrame2$`Route 12 Fuel Saved (Gallons)`
#dataFrame$newColumn <- dataFrame$oldColumn1 + dataFrame$oldColumn2
View(dataFrame2)

#create bar graph
barplot(dataFrame2$TotalFuel,
        main = "Total Fuel saved per Month ",
        xlab = "Month",
        ylab = "Fuel Saved (gallons)",
        names.arg = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."),
        col = "blue"
)
# code was completed before recording, but pushed to GitHub after.

dataFrame2$busesEliminated <- dataFrame2$`Free Trolley Buses Eliminated`+ dataFrame2$`Route 1 Buses Eliminated`+dataFrame2$`Route 2 Buses Eliminated`+dataFrame2$`Route 3 Buses Eliminated`+dataFrame2$`Route 4 Buses Eliminated`+dataFrame2$`Route 5 Buses Eliminated`+dataFrame2$`Route 6 Buses Eliminated`+dataFrame2$`Route 7 Buses Eliminated`+dataFrame2$`Route 8 Buses Eliminated`+dataFrame2$`Route 9 Buses Eliminated`+dataFrame2$`Route 10 Buses Eliminated`+dataFrame2$`Route 11 Buses Eliminated`+dataFrame2$`Route 12 Buses Eliminated`
View(dataFrame2)