#Read MechaCar data
mecha_car <- read.csv('MechaCar_mpg.csv')
#Perform mulitple linear regression with continuous data
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=mecha_car)
#Get r-squared and p-value
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=mecha_car))
#Cross check to see if the results are repeated with only the factors that provide a non-random amout of variance.
lm(mpg ~ vehicle.weight + spoiler.angle, data=mecha_car)
summary(lm(mpg ~ vehicle.weight + spoiler.angle, data=mecha_car))

#Read suspension coil data
suspension_coil <- read.csv('Suspension_Coil.csv')
#Create summary table for each lot with mean, median, standard deviation and variance.
summary_table <- suspension_coil %>%
  group_by(Manufacturing_Lot) %>%
  summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), Standard_Deviation_PSI=sd(PSI))
#Filter suspension_coil data by Lot
sample_lot1 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot1')
sample_lot2 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot2')
sample_lot3 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot3')
#Perform a one-sample t-test for each lot.
t.test(sample_lot1$PSI, mu=1500)
t.test(sample_lot2$PSI, mu=1500)
t.test(sample_lot3$PSI, mu=1500)
