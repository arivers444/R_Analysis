# MPG Regression

mpg_data <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) # read in data set

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mpg_data)) # generate multiple linear regression model summary


# Suspension Coil Summary of Multiple Linear Regression

library(tidyverse) # import tidyverse

susp_coil_data <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) # read in data set

# Summary by Lot
summarize_coil <- susp_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Std_Dev_PSI=sd(PSI)) # create summary table with multiple columns

# Total Summary
total_summary_coil <- susp_coil_data %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Std_Dev_PSI=sd(PSI))

# Suspension Coil T-Tests
# Lot 1
t.test(subset(susp_coil_data,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

# Lot 2
t.test(subset(susp_coil_data,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

# Lot 3
t.test(subset(susp_coil_data,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

# Across all Lots
t.test(susp_coil_data$PSI,mu = 1500)

