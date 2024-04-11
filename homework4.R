# BIOSTAT 620 - Homework #4
# Vikram Bala

setwd("C:/Users/vikba/Documents/Class Notes/Winter 2024/BIOSTAT 620/Homework")
library(readxl)
library(ggplot2)
library(dplyr)

### DATA PREPROCESSING ###

screentime <- read_excel("ScreenTime-hw3Q3.xlsx", sheet="ScreenTime")
baseline <- read_excel("ScreenTime-hw3Q3.xlsx", sheet="Baseline")
baseline_b <- baseline[baseline$Treatment == "B",]
merge_b <- merge(screentime, baseline_b, by="pseudo_id")

### QUESTION 2 ###

weekdays <- c("Mo", "Tu", "We", "Th", "Fr")
merge_b$X <- ifelse(merge_b$Day %in% weekdays, 1, 0)
merge_b$B <- ifelse(merge_b$Phase == "Treatment", 1, 0)
merge_b$pickups_lag <- c(NA, merge_b$Pickups[-length(merge_b$Pickups)])

### 2A ###

user2 <- merge_b[merge_b$pseudo_id == 2,]
glm2 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user2, family="poisson", offset=log(Tot.Scr.Time))
est2 <- summary(glm2)$coefficients[, 1]
se2 <- summary(glm2)$coefficients[, 2]

user3 <- merge_b[merge_b$pseudo_id == 3,]
glm3 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user3, family="poisson", offset=log(Tot.Scr.Time))
est3 <- summary(glm3)$coefficients[, 1]
se3 <- summary(glm3)$coefficients[, 2]

user4 <- merge_b[merge_b$pseudo_id == 4,]
glm4 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user4, family="poisson", offset=log(Tot.Scr.Time))
est4 <- summary(glm4)$coefficients[, 1]
se4 <- summary(glm4)$coefficients[, 2]

user5 <- merge_b[merge_b$pseudo_id == 5,]
glm5 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user5, family="poisson", offset=log(Tot.Scr.Time))
est5 <- summary(glm5)$coefficients[, 1]
se5 <- summary(glm5)$coefficients[, 2]

user8 <- merge_b[merge_b$pseudo_id == 8,]
glm8 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user8, family="poisson", offset=log(Tot.Scr.Time))
est8 <- summary(glm8)$coefficients[, 1]
se8 <- summary(glm8)$coefficients[, 2]

user15 <- merge_b[merge_b$pseudo_id == 15,]
glm15 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user15, family="poisson", offset=log(Tot.Scr.Time))
est15 <- summary(glm15)$coefficients[, 1]
se15 <- summary(glm15)$coefficients[, 2]

user16 <- merge_b[merge_b$pseudo_id == 16,]
glm16 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user16, family="poisson", offset=log(Tot.Scr.Time))
est16 <- summary(glm16)$coefficients[, 1]
se16 <- summary(glm16)$coefficients[, 2]

user18 <- merge_b[merge_b$pseudo_id == 18,]
glm18 <- glm(Pickups ~ log(pickups_lag) + B + X, data=user18, family="poisson", offset=log(Tot.Scr.Time))
est18 <- summary(glm18)$coefficients[, 1]
se18 <- summary(glm18)$coefficients[, 2]

summary_est_b <- rbind(est2, est3, est4, est5, est8, est15, est16, est18)
summary_se_b <- rbind(se2, se3, se4, se5, se8, se15, se16, se18)
summary_table_b <- data.frame(cbind(summary_est_b, summary_se_b))
colnames(summary_table_b) <- c("beta0", "beta1", "beta2", "beta3",
                               "se0", "se1", "se2", "se3")
rownames(summary_table_b) <- c("2", "3", "4", "5", "8", "15", "16", "18")

### 2B ###

meta_analysis <- function(sample_sizes, b0_ests, b1_ests, b2_ests, b3_ests,
                          serrs0, serrs1, serrs2, serrs3, num_indiv)
{
  vars0 <- sample_sizes*serrs0^2
  vars1 <- sample_sizes*serrs1^2
  vars2 <- sample_sizes*serrs2^2
  vars3 <- sample_sizes*serrs3^2

  numerator_b0 <- 0
  denominator_b0 <- 0
  numerator_b1 <- 0
  denominator_b1 <- 0
  numerator_b2 <- 0
  denominator_b2 <- 0
  numerator_b3 <- 0
  denominator_b3 <- 0

  for(i in 1:num_indiv)
  {
    numerator_b0 <- numerator_b0 + sample_sizes[i]*1/(vars0[i])*b0_ests[i]
    denominator_b0 <- denominator_b0 + sample_sizes[i]*1/(vars0[i])

    numerator_b1 <- numerator_b1 + sample_sizes[i]*1/(vars1[i])*b1_ests[i]
    denominator_b1 <- denominator_b1 + sample_sizes[i]*1/(vars1[i])

    numerator_b2 <- numerator_b2 + sample_sizes[i]*1/(vars2[i])*b2_ests[i]
    denominator_b2 <- denominator_b2 + sample_sizes[i]*1/(vars2[i])

    numerator_b3 <- numerator_b3 + sample_sizes[i]*1/(vars3[i])*b3_ests[i]
    denominator_b3 <- denominator_b3 + sample_sizes[i]*1/(vars3[i])
  }

  mu0 <- numerator_b0/denominator_b0
  mu1 <- numerator_b1/denominator_b1
  mu2 <- numerator_b2/denominator_b2
  mu3 <- numerator_b3/denominator_b3
  var0 <- 1/denominator_b0
  var1 <- 1/denominator_b1
  var2 <- 1/denominator_b2
  var3 <- 1/denominator_b3
  se0 <- sqrt(var0)/sqrt(num_indiv)
  se1 <- sqrt(var1)/sqrt(num_indiv)
  se2 <- sqrt(var2)/sqrt(num_indiv)
  se3 <- sqrt(var3)/sqrt(num_indiv)
  t0 <- mu0/se0
  t1 <- mu1/se1
  t2 <- mu2/se2
  t3 <- mu3/se3
  df <- num_indiv-1
  pval0 <- pt(t0, df, lower.tail=TRUE)
  pval1 <- pt(t1, df, lower.tail=TRUE)
  pval2 <- pt(t2, df, lower.tail=TRUE)
  pval3 <- pt(t3, df, lower.tail=TRUE)
  return(list(mu0=mu0, mu1=mu1, mu2=mu2, mu3=mu3,
              se0=se0, se1=se1, se2=se2, se3=se3,
              pval0=pval0, pval1=pval1, pval2=pval2, pval3=pval3))
}

sample_sizes_b <- c(nrow(user2), nrow(user3), nrow(user4), nrow(user5),
                    nrow(user8), nrow(user15), nrow(user16), nrow(user18))
meta_results_b <- meta_analysis(sample_sizes_b, summary_table_b$beta0,
                                summary_table_b$beta1, summary_table_b$beta2,
                                summary_table_b$beta3, summary_table_b$se0,
                                summary_table_b$se1, summary_table_b$se2,
                                summary_table_b$se3, length(sample_sizes_b))

# ### QUESTION 3 ###

# ### 3A ###

meta_analysis_adj <- function(sample_sizes, b0_ests, b1_ests, b2_ests, b3_ests,
                              b4_ests, b5_ests, b6_ests, b7_ests, serrs0,
                              serrs1, serrs2, serrs3, serrs4, serrs5,
                              serrs6, serrs7, num_indiv)
{
  vars0 <- sample_sizes*serrs0^2
  vars1 <- sample_sizes*serrs1^2
  vars2 <- sample_sizes*serrs2^2
  vars3 <- sample_sizes*serrs3^2
  vars4 <- sample_sizes*serrs4^2
  vars5 <- sample_sizes*serrs5^2
  vars6 <- sample_sizes*serrs6^2
  vars7 <- sample_sizes*serrs7^2

  numerator_b0 <- 0
  denominator_b0 <- 0
  numerator_b1 <- 0
  denominator_b1 <- 0
  numerator_b2 <- 0
  denominator_b2 <- 0
  numerator_b3 <- 0
  denominator_b3 <- 0
  numerator_b4 <- 0
  denominator_b4 <- 0
  numerator_b5 <- 0
  denominator_b5 <- 0
  numerator_b6 <- 0
  denominator_b6 <- 0
  numerator_b7 <- 0
  denominator_b7 <- 0

  for(i in 1:num_indiv)
  {
    numerator_b0 <- numerator_b0 + sample_sizes[i]*1/(vars0[i])*b0_ests[i]
    denominator_b0 <- denominator_b0 + sample_sizes[i]*1/(vars0[i])

    numerator_b1 <- numerator_b1 + sample_sizes[i]*1/(vars1[i])*b1_ests[i]
    denominator_b1 <- denominator_b1 + sample_sizes[i]*1/(vars1[i])

    numerator_b2 <- numerator_b2 + sample_sizes[i]*1/(vars2[i])*b2_ests[i]
    denominator_b2 <- denominator_b2 + sample_sizes[i]*1/(vars2[i])

    numerator_b3 <- numerator_b3 + sample_sizes[i]*1/(vars3[i])*b3_ests[i]
    denominator_b3 <- denominator_b3 + sample_sizes[i]*1/(vars3[i])

    numerator_b4 <- numerator_b4 + sample_sizes[i]*1/(vars4[i])*b4_ests[i]
    denominator_b4 <- denominator_b4 + sample_sizes[i]*1/(vars4[i])

    numerator_b5 <- numerator_b5 + sample_sizes[i]*1/(vars5[i])*b5_ests[i]
    denominator_b5 <- denominator_b5 + sample_sizes[i]*1/(vars5[i])

    numerator_b6 <- numerator_b6 + sample_sizes[i]*1/(vars6[i])*b6_ests[i]
    denominator_b6 <- denominator_b6 + sample_sizes[i]*1/(vars6[i])

    numerator_b7 <- numerator_b7 + sample_sizes[i]*1/(vars7[i])*b7_ests[i]
    denominator_b7 <- denominator_b7 + sample_sizes[i]*1/(vars7[i])
  }

  mu0 <- numerator_b0/denominator_b0
  mu1 <- numerator_b1/denominator_b1
  mu2 <- numerator_b2/denominator_b2
  mu3 <- numerator_b3/denominator_b3
  mu4 <- numerator_b4/denominator_b4
  mu5 <- numerator_b5/denominator_b5
  mu6 <- numerator_b6/denominator_b6
  mu7 <- numerator_b7/denominator_b7
  var0 <- 1/denominator_b0
  var1 <- 1/denominator_b1
  var2 <- 1/denominator_b2
  var3 <- 1/denominator_b3
  var4 <- 1/denominator_b4
  var5 <- 1/denominator_b5
  var6 <- 1/denominator_b6
  var7 <- 1/denominator_b7
  se0 <- sqrt(var0)/sqrt(num_indiv)
  se1 <- sqrt(var1)/sqrt(num_indiv)
  se2 <- sqrt(var2)/sqrt(num_indiv)
  se3 <- sqrt(var3)/sqrt(num_indiv)
  se4 <- sqrt(var4)/sqrt(num_indiv)
  se5 <- sqrt(var5)/sqrt(num_indiv)
  se6 <- sqrt(var6)/sqrt(num_indiv)
  se7 <- sqrt(var7)/sqrt(num_indiv)
  t0 <- mu0/se0
  t1 <- mu1/se1
  t2 <- mu2/se2
  t3 <- mu3/se3
  t4 <- mu4/se4
  t5 <- mu5/se5
  t6 <- mu6/se6
  t7 <- mu7/se7
  df <- num_indiv-1
  pval0 <- pt(t0, df, lower.tail=TRUE)
  pval1 <- pt(t1, df, lower.tail=TRUE)
  pval2 <- pt(t2, df, lower.tail=TRUE)
  pval3 <- pt(t3, df, lower.tail=TRUE)
  pval4 <- pt(t4, df, lower.tail=TRUE)
  pval5 <- pt(t5, df, lower.tail=TRUE)
  pval6 <- pt(t6, df, lower.tail=TRUE)
  pval7 <- pt(t7, df, lower.tail=TRUE)
  return(list(mu0=mu0, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4, mu5=mu5,
              mu6=mu6, mu7=mu7, se0=se0, se1=se1, se2=se2, se3=se3,
              se4=se4, se5=se5, se6=se6, se7=se7, pval0=pval0,
              pval1=pval1, pval2=pval2, pval3=pval3, pval4=pval4,
              pval5=pval5, pval6=pval6, pval7=pval7))
}

### 3B ###

merge_ab <- merge(screentime, baseline, by="pseudo_id")
weekdays <- c("Mo", "Tu", "We", "Th", "Fr")
merge_ab$X <- ifelse(merge_ab$Day %in% weekdays, 1, 0)
merge_ab$pickups_lag <- c(NA, merge_ab$Pickups[-length(merge_ab$Pickups)])
merge_ab$A <- ifelse(merge_ab$Treatment=="A" & merge_ab$Phase == "Treatment", 1, 0)
merge_ab$B <- ifelse(merge_ab$Treatment=="B" & merge_ab$Phase == "Treatment", 1, 0)

glm_aba <- glm(Pickups ~ log(pickups_lag) + A + X + sex + age + pets + siblings,
               data=merge_ab, family="poisson", offset=log(Tot.Scr.Time))
est_aba <- summary(glm_aba)$coefficients[, 1]
se_aba <- summary(glm_aba)$coefficients[, 2]

glm_abb <- glm(Pickups ~ log(pickups_lag) + B + X + sex + age + pets + siblings,
               data=merge_ab, family="poisson", offset=log(Tot.Scr.Time))
est_abb <- summary(glm_abb)$coefficients[, 1]
se_abb <- summary(glm_abb)$coefficients[, 2]

summary_est_ab <- rbind(est_aba, est_abb)
summary_se_ab <- rbind(se_aba, se_abb)
summary_table_ab <- data.frame(cbind(summary_est_ab, summary_se_ab))
colnames(summary_table_ab) <- c("beta0", "beta1", "beta2", "beta3", "beta4",
                                "beta5", "beta6", "beta7", "se0", "se1",
                                "se2", "se3", "se4", "se5", "se6", "se7")
rownames(summary_table_ab) <- c("A", "B")

sample_sizes_ab <- c(nrow(merge_ab), nrow(merge_ab))
mres_ab <- meta_analysis_adj(sample_sizes_ab, summary_table_ab$beta0,
                             summary_table_ab$beta1, summary_table_ab$beta2,
                             summary_table_ab$beta3, summary_table_ab$beta4,
                             summary_table_ab$beta5, summary_table_ab$beta6,
                             summary_table_ab$beta7, summary_table_ab$se0,
                             summary_table_ab$se1, summary_table_ab$se2,
                             summary_table_ab$se3, summary_table_ab$se4,
                             summary_table_ab$se5, summary_table_ab$se6,
                             summary_table_ab$se7, length(sample_sizes_ab))

### 3C ###

merge_ab$R <- ifelse((merge_ab$Treatment == "A" | merge_ab$Treatment == "B") &
                       merge_ab$Phase == "Treatment", 1, 0)
glm_fin <- glm(Pickups ~ log(pickups_lag) + R + X + sex + age + pets + siblings,
               data=merge_ab, family="poisson", offset=log(Tot.Scr.Time))
summary(glm_fin)
