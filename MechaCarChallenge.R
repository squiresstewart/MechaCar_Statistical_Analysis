library(tidyverse)

demo_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

Mecha_car_model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=demo_table)

Mecha_sum <- summary(lm(Mecha_car_model))

coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

total_summary <- coil_tale %>% summarize(PSI_Mean=mean(PSI),PSI_Median=median(PSI),PSI_Var=var(PSI),PSI_sd=sd(PSI))

lot_summary <- coil_tale %>% group_by(Manufacturing_Lot) %>% summarize(PSI_Mean=mean(PSI),PSI_Median=median(PSI),PSI_Var=var(PSI),PSI_sd=sd(PSI))

t.test(coil_tale$PSI,mu=1500)

lot1 <- subset(coil_tale,Manufacturing_Lot=="Lot1")

t.test(lot1$PSI,mu=1500)

lot2 <- subset(coil_tale,Manufacturing_Lot=="Lot2")

t.test(lot2$PSI,mu=1500)

lot3 <- subset(coil_tale,Manufacturing_Lot=="Lot3")

t.test(lot3$PSI,mu=1500)

