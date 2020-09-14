# MPG Regression

mpg_data <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) # read in data set

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=mpg_data)) # generate multiple linear regression model summary


# Suspension Coil Summary of Multiple Linear Regression

library(tidyverse) # import tidyverse

susp_coil_data <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) # read in data set

summarize_coil <- susp_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Std_Dev_PSI=sd(PSI)) # create summary table with multiple columns


# Suspension Coil T-Test, One-Sample

t.test(log10(susp_coil_data$PSI),mu=log10(1500)) # compare sample versus population means
