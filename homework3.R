# BIOSTAT 620 - Homework #3
# Vikram Bala

setwd("C:/Users/vikba/Documents/Class Notes/Winter 2024/BIOSTAT 620/Homework")
library(readxl)
library(ggplot2)
library(dplyr)
library(tableone)
library(Matching)
library(boot)

### DATA PREPROCESSING ###

screentime <- read_excel("ScreenTime-hw3Q3.xlsx", sheet="ScreenTime")
baseline <- read_excel("ScreenTime-hw3Q3.xlsx", sheet="Baseline")
merge <- merge(screentime, baseline, by="pseudo_id")

### QUESTION 3 ###

source("hw3_sourcing.R")
merge_ap <- merge[merge$Treatment != "B",]
merge_bp <- merge[merge$Treatment != "A",]
merge_ap$Treatment <- ifelse(merge_ap$Treatment == "A", 1, 0)
merge_bp$Treatment <- ifelse(merge_bp$Treatment == "B", 1, 0)

formula <- Treatment ~ sex + age + pets + siblings
ap_pscmat_social <- propensity_score_match(merge_ap, formula, 8, 4)
bp_pscmat_social <- propensity_score_match(merge_bp, formula, 8, 4)

prop_ap_model <- glm(formula, family=binomial(), data=merge_ap)
prop_ap_fitted <- prop_ap_model$fitted
rpt_analysis_ap <- Match(Tr=merge_ap$Treatment, M=1, X=logit(prop_ap_fitted), replace=FALSE, caliper=0.1)
matched_out_ap <- merge_ap[unlist(rpt_analysis_ap[c("index.treated","index.control")]),]
var <- c("Tot.Soc.Time")
matched_tab_ap <- CreateTableOne(vars=var, strata="Treatment", data=matched_out_ap, test=TRUE)
print(matched_tab_ap, smd=TRUE)

prop_bp_model <- glm(formula, family=binomial(), data=merge_bp)
prop_bp_fitted <- prop_bp_model$fitted
rpt_analysis_bp <- Match(Tr=merge_bp$Treatment, M=1, X=logit(prop_bp_fitted), replace=FALSE, caliper=0.1)
matched_out_bp <- merge_bp[unlist(rpt_analysis_bp[c("index.treated","index.control")]),]
var <- c("Tot.Soc.Time")
matched_tab_bp <- CreateTableOne(vars=var, strata="Treatment", data=matched_out_bp, test=TRUE)
print(matched_tab_bp, smd=TRUE)

### QUESTION 4 ###

merge_sub <- merge[merge$Treatment != "P",]
merge_sub$Treatment <- ifelse(merge_sub$Treatment == "A", 1, 0)

reg_model <- lm(Tot.Soc.Time ~ Treatment + sex + age + pets + siblings, data=merge_sub)
summary(reg_model)

ipw_model <- glm(Treatment ~ sex + age + pets + siblings,
                 family=binomial(link="logit"), data=merge_sub)
propens <- ipw_model$fitted
w <- 1/propens
wbar <- 1/(1-propens)
ipw_diff_treat <- sum(w*merge_sub$Treatment*merge_sub$Tot.Soc.Time)/sum(w*merge_sub$Treatment) -
  sum(wbar*(1-merge_sub$Treatment)*merge_sub$Tot.Soc.Time)/sum(wbar*(1-merge_sub$Treatment))
print(ipw_diff_treat)
