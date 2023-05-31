library(devtools)
library(drc)
library(bmd)
library(ggplot2)
library(plyr)
library(readr)
library(tidyverse)
library(wesanderson)
library(ggpubr)
library(cowplot)
library(gridtext)

#Import dataset
data.comp48 <- read.csv("Data/Comp_48h.csv")
data.comp36 <- read.csv("Data/Comp_36h.csv")
data.comp24 <- read.csv("Data/Comp_24h.csv")

##48h
#Fit drcs to age groups in molting
molt48 <- drm(Molting ~ Conc, Age, data = data.comp48, fct = LL.4(fixed = c(NA,0,100,NA)))

summary(molt48) 

#Fit drcs to age groups in survival
surv48 <- drm(Survival ~ Conc, Age, data = data.comp48, fct = LL.4(fixed = c(NA,0,100,NA)))

summary(surv48)            

# Compare parameters molt
# Slope

CM48_Slope <- data.frame(compParm(molt48, "b", "-"))

# EC50

CM48_EC50 <- data.frame(compParm(molt48, "e", "-"))

# Compare parameters surv
# Slope

CS48_Slope <- data.frame(compParm(surv48, "b", "-"))

# EC50

CS48_EC50 <- data.frame(compParm(surv48, "e", "-"))

##36h
#Fit drcs to age groups in molting
molt36 <- drm(Molting ~ Conc, Age, data = data.comp36, fct = LL.4(fixed = c(NA,0,100,NA)))

summary(molt36) 

#Fit drcs to age groups in survival
surv36 <- drm(Survival ~ Conc, Age, data = data.comp36, fct = LL.4(fixed = c(NA,0,100,NA)))

summary(surv36)            

# Compare parameters molt
# Slope

CM36_Slope <- data.frame(compParm(molt36, "b", "-"))

# EC50

CM36_EC50 <- data.frame(compParm(molt36, "e", "-"))

# Compare parameters surv
# Slope

CS36_Slope <- data.frame(compParm(surv36, "b", "-"))

# EC50

CS36_EC50 <- data.frame(compParm(surv36, "e", "-"))

write.csv(CM36_EC50, "Results/CM36_EC50.csv")

write.csv(CS36_EC50, "Results/CS36_EC50.csv")

write.csv(CM36_Slope, "Results/CM36_Slope.csv")

write.csv(CS36_Slope, "Results/CS36_Slope.csv")

write.csv(CM48_EC50, "Results/CM48_EC50.csv")

write.csv(CS48_EC50, "Results/CS48_EC50.csv")

write.csv(CM48_Slope, "Results/CM48_Slope.csv")

write.csv(CS48_Slope, "Results/CS48_Slope.csv")
