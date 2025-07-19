library(tidyverse)
library(vtable)
library(lubridate)
library(DescTools)
library(lmtest)
library(sandwich)
library(stargazer)

setwd("C:/Users/kjcol/Desktop/Data Visualization")

military_spending_data <- read.csv("Military Expenditure.csv")

glimpse(military_spending_data)

