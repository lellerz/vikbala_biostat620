# BIOSTAT 620 - Homework #2
# Vikram Bala

setwd("C:/Users/vikba/Documents/Class Notes/Winter 2024/BIOSTAT 620")
library(readxl)
library(ggplot2)
library(dplyr)
library(systemfit)
library(car)

### Preprocess Data ###
data <- read_excel("ScreenTime_VBala.xlsx")
data$DPSST <- data$Social.ST.min/data$Total.ST.min
data$DDPU <- data$Total.ST.min/data$Pickups
data$Pickup.1st <- gsub('1899-12-31 ', '', data$Pickup.1st)
data$Date <- as.Date(as.POSIXct(data$Date, 'EST'))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data$X <- ifelse(weekdays(data$Date) %in% weekdays, 1, 0)
data$Z <- ifelse(data$Date < as.Date("2024-01-10"), 0, 1)

### Problem 2 ###
data$total_lag <- c(NA, data$Total.ST.min[-length(data$Total.ST.min)])
data$social_lag <- c(NA, data$Social.ST.min[-length(data$Social.ST.min)])
y1 <- Total.ST.min ~ X + Z + total_lag
y2 <- Social.ST.min ~ X + Z + social_lag
sur_model <- systemfit(list(total=y1, social=y2), data=data, method="SUR")
summary(sur_model)
hyp_test <- linearHypothesis(sur_model, c("total_Z=0", "social_Z=0"))
print(hyp_test)
