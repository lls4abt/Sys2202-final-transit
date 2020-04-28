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

