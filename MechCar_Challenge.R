install.packages("tidyverse")
library(tidyverse)
library(jsonlite)
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
head(MechaCar_mpg)

# Rename columns of the dataset
MechaCar_mpg <- MechaCar_mpg %>% 
  rename(
    vlength = "vehicle length", 
    vweight = "vehicle weight", 
    spangle = "spoiler angle", 
    grclearance = "ground clearance")
head(MechaCar_mpg)

# Using multiple linear regression, design a linear model that predicts the mpg of MechaCar prototypes using a number of variables within the MechaCar mpg dataset.
lm(formula = mpg ~ vlength + vweight + spangle + grclearance + AWD, data = MechaCar_mpg)

# Create summary statistics
summary(lm(mpg ~ vlength + vweight + spangle + grclearance + AWD,MechaCar_mpg))


# Reading the Suspension Coil dataset
susp_coil <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)

# Visualizing PSI distribution using density plot
ggplot(susp_coil,aes(x=PSI)) + geom_density()


shapiro.test(susp_coil$PSI)
.

summary(susp_coil$PSI)
var(susp_coil$PSI) 
sd(susp_coil$PSI)

# Suspension Coil T-Test
t.test(x=susp_coil$PSI,mu=1500)
