######## biostat 620 project 1 #######
####### Lufeiya Liu, Anran Yao #######

#### packages ####
library(dplyr)
library(tidyverse)
library(gtsummary)
library(GGally) # for correlation plots
library(kableExtra)
library(ggplot2)
library(hms)
library(circular)
library(car)
library(readxl)


#### read the data ####
timedata<-read_excel("VENTURE_baseline_data_hw3.xlsx", sheet="combined_raw_data")
date <- as.Date("2024/01/03")
date <- rep(seq(date, by="day", length.out=42), 3)
timedata$Date<-date
baseline<-read_excel("VENTURE_baseline_data_hw3.xlsx", sheet="covariate_data")
baseline<-baseline[, -ncol(baseline)]

# change the format of the 1st pickup to time format, transform to angular
timedata <- timedata %>%
  mutate(Pickup.1st = strptime(Pickup.1st, format = "%H:%M"),
         Pickup.1st.angular =
           (hour(Pickup.1st)*60+minute(Pickup.1st))/(24*60)*360)

# combine the data with baseline
combined <- timedata %>% left_join(baseline, by = c("ID"="ID"))

#### data validation ####
hm_to_min = function(hm){
  unlist(lapply(hm,function(x){
    if(grepl("h", x) && !grepl("m", x)) {
      x = gsub("h", "*60", x)
      return(eval(parse(text = x)))
    }else if(grepl("h", x)){
      x = gsub("h","*60+",x)
      x = gsub("m","",x)
      return(eval(parse(text=x)));
    }else{
      return(as.numeric(gsub("m","",x)))
    }})) #convert to total minutes
}

validation = timedata %>%
  mutate(Total.ST.min.true = hm_to_min(Total.ST),
         Social.ST.min.true = hm_to_min(Social.ST),
         Total.ST.match = Total.ST.min.true==Total.ST.min,
         Social.ST.match = Social.ST.min.true == Social.ST.min)%>%
  relocate(Date,Total.ST,Total.ST.min,Total.ST.min.true,Total.ST.match,
           Social.ST,Social.ST.min,Social.ST.min.true,Social.ST.match)
validation$Total.ST.match
validation$Social.ST.match
combined_validated <- validation %>% left_join(baseline, by = c("ID"="ID"))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
combined_validated$Weekdays <- ifelse(weekdays(combined_validated$Date) %in% weekdays, 1, 0)
# write.csv(combined_validated,"combined_validated.csv")

#### descriptive statistics ####
colnames(baseline)
baseline_summary <- baseline %>%
  select(-ID) %>%
  mutate(total = 1) %>%
  select(total,everything())%>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    digits = all_continuous() ~ 1,
    type = list(
      workmate ~ "continuous",
      academic ~ "continuous",
      `non-academic` ~ "continuous",
      pets ~ "continuous",
      sex ~ "categorical",
      age ~ "continuous",
      `course hours` ~ "continuous",
      degree ~ "categorical",
      job ~ "categorical",
      siblings ~ "continuous",
      apps ~ "continuous",
      devices ~ "continuous",
      procrastination ~ "continuous"
    ),
    missing = "no") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Summary Statistics**")

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
timedata$Weekdays <- ifelse(weekdays(timedata$Date) %in% weekdays, 1, 0)
#data$Z <- ifelse(data$Date < as.Date("2024-01-10"), 0, 1)

timedata_summary <- timedata %>%
  select(Total.ST.min,Social.ST.min,Pickups,Pickup.1st.angular,
         DPSST,DDPU,Weekdays)%>%
  mutate(total = 1) %>%
  select(total,everything())%>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    type = Weekdays ~ "categorical",
    digits = all_continuous() ~ 1,
    missing = "no") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Summary Statistics**")

tbl_all <-tbl_stack(tbls = list(baseline_summary, timedata_summary))
summary_latex <-tbl_all %>% as_gt() %>%
  gt::as_latex()
cat(summary_latex[1], sep = '\n')

#### plots ######
#### correlation ####
timedata %>%
  ggpairs(columns = c(4,6,7,9,10,11,13), progress = FALSE)+ theme_bw()

# first.pickup.circular distribution
layout(1)
first.pickup.cir = circular(timedata$Pickup.1st.angular, units="degrees",
                            template="clock24")
first.pickup.cir = na.omit(first.pickup.cir)
first.pickup.cir.den = density(first.pickup.cir,bw=50)
plot(first.pickup.cir.den, points.plot = T,shrink=1.3, main = "")

#### boxplots#######
layout(matrix(c(1,2,3,4,5), nrow = 1))
boxplot(Total.ST.min~ID,timedata,ylab="Total Screen Time (min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray", las=2)
boxplot(Social.ST.min~ID,timedata,ylab="Social Screen Time (min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray", las=2)
boxplot(Pickups~ID,timedata,ylab="Number of Pickups",
        xlab="Individual",all.outliers=TRUE,col = "lightgray", las=2)
boxplot(DPSST~ID,timedata,
        ylab="Daily Social Apps Screen Time Proportion",
        xlab="Individual",all.outliers=TRUE,col = "lightgray", las=2)
boxplot(DDPU~ID,timedata,ylab="Daily Duration Use(min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray", las=2)


#### federated learning ####
combined1 <- combined_validated %>%
  select(-Total.ST,-Total.ST.min,-Total.ST.match,
         -Social.ST,-Social.ST.min,-Social.ST.match) %>%
  mutate(Total.ST.min=Total.ST.min.true,
         Social.ST.min=Social.ST.min.true)
combined1$Social.ST.min[75] <- 75

# data for person 1
timedata1 <- subset(combined1, ID=="vikbala")
# data for person 2
timedata2 <- subset(combined1, ID=="eawerner")
# data for person 3
timedata3 <- subset(combined1, ID=="neyan")

x1 = model.matrix(Total.ST.min ~ Pickups + Num.Classes + DPSST, data=timedata1)
x2 = model.matrix(Total.ST.min ~ Pickups + Num.Classes + DPSST, data=timedata2)
x3 = model.matrix(Total.ST.min ~ Pickups + Num.Classes + DPSST, data=timedata3)

SSX1 = t(x1)%*%x1
SSXY1 = t(x1)%*%timedata1$Total.ST.min
SSY1 = t(timedata1$Total.ST.min) %*% timedata1$Total.ST.min

SSX2 = t(x2)%*%x2
SSXY2 = t(x2)%*%timedata2$Total.ST.min
SSY2 = t(timedata2$Total.ST.min) %*% timedata2$Total.ST.min

SSX3 = t(x3)%*%x3
SSXY3 = t(x3)%*%timedata3$Total.ST.min
SSY3 = t(timedata3$Total.ST.min) %*% timedata3$Total.ST.min

# beta hat estimates
beta_y <- solve(SSX1+SSX2+SSX3)%*%(SSXY1+SSXY2+SSXY3)

sigma_y <-((SSY1+SSY2+SSY3)-2*t(beta_y)%*%(SSXY1+SSXY2+SSXY3)+
             t(beta_y)%*%(SSX1+SSX2+SSX3)%*%(beta_y))/(126-3)
ste <- sqrt(sigma_y)
# standard errors
se_beta = t(ste%*%sqrt(diag(as.matrix(solve(as.matrix(SSX1+SSX2+SSX3))))))
# t statistics
t_statistic = beta_y/se_beta

# p values
p_value = c(2*(1-pt(q=abs(t_statistic),df=123)))

# federal results
fed_table = data.frame(beta_y=beta_y, se_beta=se_beta, t_statistic, p_value)
fed_table

# calculate RSS
RSS = ((SSY1+SSY2+SSY3)-2*t(beta_y)%*%(SSXY1+SSXY2+SSXY3)+
         t(beta_y)%*%(SSX1+SSX2+SSX3)%*%(beta_y))

# calculate R^2a
y_bar1 = mean(timedata1$Total.ST.min)
y_bar2 = mean(timedata2$Total.ST.min)
y_bar3 = mean(timedata3$Total.ST.min)

y_bar = 42/126*y_bar1+42/126*y_bar2+42/126*y_bar3
TSS = (SSY1+SSY2+SSY3)-126*y_bar^2
Ra_2 = 1-RSS/(126-3)/(TSS/(126-1))
Ra_2 # 0.1790536

#### confirmation analysis ####
# lm_1 <- lm(Social.ST.min~Pickup.1st.angular
#            +Pickups
#            +Weekdays
#            +apps , data=combined1)

lm_1 <- lm(Total.ST.min ~ Pickups + Num.Classes + DPSST, data=combined1)

(lm_1.sum<-summary(lm_1))
anova(lm_1) # see RSS

# get tables
# kbl(lm_1.sum$coefficients,format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)

##### model diagnosis #####
hist(lm_1$residuals,main = "",xlab = "Residuals")
vif(lm_1)%>% kbl(format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)
par(mfrow = c(1, 2))
plot(lm_1,c(1,2))
