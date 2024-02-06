# BIOSTAT 620 - Homework #1
# Vikram Bala

setwd("C:/Users/vikba/Documents/Class Notes/Winter 2024/BIOSTAT 620")
library(readxl)
library(ggplot2)
library(dplyr)
library(circular)

### Problem 1 ###
data <- read_excel("ScreenTime_VBala.xlsx")

# 1D #
data$DPSST <- data$Social.ST.min/data$Total.ST.min
data$DDPU <- data$Total.ST.min/data$Pickups

### Problem 2 ###
data$Pickup.1st <- gsub('1899-12-31 ', '', data$Pickup.1st)
data$Date <- as.Date(as.POSIXct(data$Date, 'EST'))

# 2A #
ggplot(aes(x=Date, y=Total.ST.min), data=data) + geom_line()
ggplot(aes(x=Date, y=Social.ST.min), data=data) + geom_line()
ggplot(aes(x=Date, y=Pickups), data=data) + geom_line()
ggplot(aes(x=Date, y=DPSST), data=data) + geom_line()
ggplot(aes(x=Date, y=DDPU), data=data) + geom_line()

# 2B #
var_data <- select(data, c('Total.ST.min','Social.ST.min',
                           'Pickups','DPSST','DDPU'))
plot(var_data)
cor(var_data, method='pearson')

# 2C #

# Total Screen Time
probs_total <- c()
total_vals <- seq(from=0, to=max(data$Total.ST.min)+10, by=10)
for(i in seq_along(total_vals))
{
  probs_total[i] <- length(which(data$Total.ST.min > total_vals[[i]]))/nrow(data)
}
plot(total_vals, probs_total)

# Social Screen Time
probs_social <- c()
social_vals <- seq(from=0, to=max(data$Social.ST.min)+10, by=10)
for(i in seq_along(social_vals))
{
  probs_social[i] <- length(which(data$Social.ST.min > social_vals[[i]]))/nrow(data)
}
plot(social_vals, probs_social)

# Number of Pickups
probs_pickups <- c()
pickups_vals <- seq(from=0, to=max(data$Pickups)+2, by=2)
for(i in seq_along(pickups_vals))
{
  probs_pickups[i] <- length(which(data$Pickups > pickups_vals[[i]]))/nrow(data)
}
plot(pickups_vals, probs_pickups)

# Daily Proportion of Social Screen Time
probs_dpsst <- c()
dpsst_vals <- seq(from=0, to=max(data$DPSST)+0.025, by=0.025)
for(i in seq_along(dpsst_vals))
{
  probs_dpsst[i] <- length(which(data$DPSST > dpsst_vals[[i]]))/nrow(data)
}
plot(dpsst_vals, probs_dpsst)

# Daily Duration Per Use
probs_ddpu <- c()
ddpu_vals <- seq(from=0, to=max(data$DDPU)+0.5, by=0.5)
for(i in seq_along(ddpu_vals))
{
  probs_ddpu[i] <- length(which(data$DDPU > ddpu_vals[[i]]))/nrow(data)
}
plot(ddpu_vals, probs_ddpu)

# 2D #

# Total Screen Time
total_acf <- acf(data$Total.ST.min, plot=FALSE, type="correlation")
acf(data$Total.ST.min, type="correlation")

# Social Screen Time
social_acf <- acf(data$Social.ST.min, plot=FALSE, type="correlation")
acf(data$Social.ST.min, type="correlation")

# Number of Pickups
pickups_acf <- acf(data$Pickups, plot=FALSE, type="correlation")
acf(data$Pickups, type="correlation")

# Daily Proportion of Social Screen Time
dpsst_acf <- acf(data$DPSST, plot=FALSE, type="correlation")
acf(data$DPSST, type="correlation")

# Daily Duration Per Use
ddpu_acf <- acf(data$DDPU, plot=FALSE, type="correlation")
acf(data$DDPU, type="correlation")

### Problem 3 ###

# 3A #
hour_vals <- as.numeric(substr(data$Pickup.1st, start=1, stop=2))
mins_vals <- as.numeric(substr(data$Pickup.1st, start=4, stop=5))
data$Angular.Pickup.1st <- ((hour_vals*60 + mins_vals)/(24*60))*360

# 3B #
circle <- circular(data$Angular.Pickup.1st, units="degrees", template="clock24")
plot(circle)

# 3C #
plot(circle, stack=TRUE, bins=48)

### Problem 4 ###

# 4B #
model1 <- glm(Pickups ~ 1, data=data, family="poisson", offset=log(Total.ST.min/60))
model1_lambda_estimate <- exp(model1$coefficients)

# 4C #
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data$X <- ifelse(weekdays(data$Date) %in% weekdays, 1, 0)
data$Z <- ifelse(data$Date < as.Date("2024-01-10"), 0, 1)
model2 <- glm(Pickups ~ X+Z, data=data, family="poisson", offset=log(Total.ST.min/60))
model2_lambda_estimate <- exp(model2$coefficients[1])
summary(model2)

### Problem 5 ###

# 5A #
mle_von <- mle.vonmises(circle)

# 5B #
sample_time <- ((8*60 + 30)/(24*60))*360
sample_time <- circular(sample_time, units="degrees", template="clock24")
p_von <- pvonmises(sample_time, mle_von$mu, mle_von$kappa)
p_von_final <- 1-p_von
