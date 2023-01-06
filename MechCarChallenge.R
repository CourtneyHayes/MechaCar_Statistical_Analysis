library(tidyverse)
mechadata <- read.csv('MechaCar_mpg.csv')

mechacar_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = mechadata)

summary(mechacar_lm)


susp_coil <- read.csv('Suspension_Coil.csv')

total_summary <- susp_coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

lot_summary <- susp_coil %>% group_by(Manufacturing_Lot) %>%summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

t.test(Mean$lot_summary, 1500, paired = T)