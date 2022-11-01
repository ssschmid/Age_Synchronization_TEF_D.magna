install.packages("wesanderson")
install.packages("ggpubr")
install.packages("gridtext")
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

Inf_age_BMD <- read.csv("C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis/BMD Analysis/Data/Inf_age_BMD.csv")

###Survival
##48h
#0-4h
#CV

CV_48S_0_4 <- (sd(Inf_age_BMD$S_04_48)/mean(Inf_age_BMD$S_04_48))

#Fit log logistic Model 0-4h

lm_48S_0_4 <- drm(S_04_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48S_0_4)

summary(lm_48S_0_4)

bmd(lm_48S_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48S_0_4, level = 0.95)

#4-8h
#CV

CV_48S_4_8 <- (sd(Inf_age_BMD$S_48_48)/mean(Inf_age_BMD$S_48_48))

#Fit log logistic Model 0-4h

lm_48S_4_8 <- drm(S_48_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48S_4_8)

summary(lm_48S_4_8)

bmd(lm_48S_4_8, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48S_4_8, level = 0.95)

#8-12h
#CV

CV_48S_8_12 <- (sd(Inf_age_BMD$S_812_48)/mean(Inf_age_BMD$S_812_48))

#Fit log logistic Model 0-4h

lm_48S_8_12 <- drm(S_812_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48S_8_12)

summary(lm_48S_8_12)

bmd(lm_48S_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48S_8_12, level = 0.95)

#0-12h
#CV

CV_48S_0_12 <- (sd(Inf_age_BMD$S_012_48)/mean(Inf_age_BMD$S_012_48))

#Fit log logistic Model 0-4h

lm_48S_0_12 <- drm(S_012_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48S_0_12)

summary(lm_48S_0_12)

bmd(lm_48S_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48S_0_12, level = 0.95)


#0-24h
#CV

CV_48S_0_24 <- (sd(Inf_age_BMD$S_024_48)/mean(Inf_age_BMD$S_024_48))

#Fit log logistic Model 0-4h

lm_48S_0_24 <- drm(S_024_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48S_0_24)

summary(lm_48S_0_24)

bmd(lm_48S_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48S_0_24, level = 0.95)


###Survival
##36
#0-4h
#Fit log logistic Model 0-4h

lm_36S_0_4 <- drm(S_04_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36S_0_4)

summary(lm_36S_0_4)

bmd(lm_36S_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36S_0_4, level = 0.95)

#4-8h
#Fit log logistic Model 0-4h

lm_36S_4_8 <- drm(S_48_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36S_4_8)

summary(lm_36S_4_8)

bmd(lm_36S_4_8, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36S_4_8, level = 0.95)

#8-12h
#Fit log logistic Model 0-4h

lm_36S_8_12 <- drm(S_812_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36S_8_12)

summary(lm_36S_8_12)

bmd(lm_36S_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36S_8_12, level = 0.95)

#0-12h
#Fit log logistic Model 0-4h

lm_36S_0_12 <- drm(S_012_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36S_0_12)

summary(lm_36S_0_12)

bmd(lm_36S_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36S_0_12, level = 0.95)

#0-24h
#Fit log logistic Model 0-4h

lm_36S_0_24 <- drm(S_024_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36S_0_24)

summary(lm_36S_0_24)

bmd(lm_36S_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36S_0_24, level = 0.95)

###Survival
##24
#0-4h
#Fit log logistic Model 0-4h

lm_24S_0_4 <- drm(S_04_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24S_0_4)

summary(lm_24S_0_4)

bmd(lm_24S_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24S_0_4, level = 0.95)

#4-8h
#Fit log logistic Model 0-4h

lm_24S_4_8 <- drm(S_48_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24S_4_8)

summary(lm_24S_4_8)

bmd(lm_24S_4_8, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24S_4_8, level = 0.95)

#8-12h
#Fit log logistic Model 0-4h

lm_24S_8_12 <- drm(S_812_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24S_8_12)

summary(lm_24S_8_12)

bmd(lm_24S_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24S_8_12, level = 0.95)

#0-12h
#Fit log logistic Model 0-4h

lm_24S_0_12 <- drm(S_012_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24S_0_12)

summary(lm_24S_0_12)

bmd(lm_24S_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24S_0_12, level = 0.95)

#0-24h
#Fit log logistic Model 0-4h

lm_24S_0_24 <- drm(S_024_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24S_0_24)

summary(lm_24S_0_24)

bmd(lm_24S_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24S_0_24, level = 0.95)

###Molting
#48h
#0-4h
#Fit log logistic Model 0-4h

lm_48M_0_4 <- drm(M_04_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48M_0_4)

summary(lm_48M_0_4)

bmd(lm_48M_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48M_0_4, level = 0.95)

#4-8h
#Fit log logistic Model 0-4h

lm_48M_4_8 <- drm(M_48_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48M_4_8)

summary(lm_48M_4_8)

confint(lm_48M_4_8, level = 0.95)

#8-12h
#Fit log logistic Model 0-4h

lm_48M_8_12 <- drm(M_812_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48M_8_12)

summary(lm_48M_8_12)

bmd(lm_48M_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48M_8_12, level = 0.95)

#0-12h
#Fit log logistic Model 0-4h

lm_48M_0_12 <- drm(M_012_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48M_0_12)

summary(lm_48M_0_12)

bmd(lm_48M_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48M_0_12, level = 0.95)

#0-24h
#Fit log logistic Model 0-4h

lm_48M_0_24 <- drm(M_024_48 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_48M_0_24)

summary(lm_48M_0_24)

bmd(lm_48M_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_48M_0_24, level = 0.95)

###Molting
##36
#0-4h
#Fit log logistic Model 0-4h

lm_36M_0_4 <- drm(M_04_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36M_0_4)

summary(lm_36M_0_4)

bmd(lm_36M_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36M_0_4, level = 0.95)

#4-8h
#Fit log logistic Model 0-4h

lm_36M_4_8 <- drm(M_48_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36M_4_8)

summary(lm_36M_4_8)

bmd(lm_36M_4_8, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36M_4_8, level = 0.95)

#8-12h
#Fit log logistic Model 0-4h

lm_36M_8_12 <- drm(M_812_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36M_8_12)

summary(lm_36M_8_12)

bmd(lm_36M_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36M_8_12, level = 0.95)

#0-12h
#Fit log logistic Model 0-4h

lm_36M_0_12 <- drm(M_012_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36M_0_12)

summary(lm_36M_0_12)

bmd(lm_36M_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36M_0_12, level = 0.95)

#0-24h
#Fit log logistic Model 0-4h

lm_36M_0_24 <- drm(M_024_36 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_36M_0_24)

summary(lm_36M_0_24)

bmd(lm_36M_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_36M_0_24, level = 0.95)

###Survival
##24
#0-4h
#Fit log logistic Model 0-4h

lm_24M_0_4 <- drm(M_04_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24M_0_4)

summary(lm_24M_0_4)

bmd(lm_24M_0_4, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24M_0_4, level = 0.95)

#4-8h
#Fit log logistic Model 0-4h

lm_24M_4_8 <- drm(M_48_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24M_4_8)

summary(lm_24M_4_8)

bmd(lm_24M_4_8, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24M_4_8, level = 0.95)

#8-12h
#Fit log logistic Model 0-4h

lm_24M_8_12 <- drm(M_812_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24M_8_12)

summary(lm_24M_8_12)

bmd(lm_24M_8_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24M_8_12, level = 0.95)

#0-12h
#Fit log logistic Model 0-4h

lm_24M_0_12 <- drm(M_012_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24M_0_12)

summary(lm_24M_0_12)

bmd(lm_24M_0_12, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24M_0_12, level = 0.95)

#0-24h
#Fit log logistic Model 0-4h

lm_24M_0_24 <- drm(M_024_24 ~ Conc, data = Inf_age_BMD, fct = LL.4(fixed = c(NA,0,100,NA), names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

plot(lm_24M_0_24)

summary(lm_24M_0_24)

bmd(lm_24M_0_24, bmr = 0.05, backgType = "modelBased", def = "relative")

confint(lm_24M_0_24, level = 0.95)

###Predict curves for use with ggplot
##Survival 24h 0-4h

# new dose levels as support for the line

newdata.Surv24h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv24h_0_4 <- predict(lm_24S_0_4, newdata = newdata.Surv24h_0_4, interval = "confidence")

# new data with predictions

newdata.Surv24h_0_4$p <- pm.Surv24h_0_4[,1]
newdata.Surv24h_0_4$pmin <- pm.Surv24h_0_4[,2]
newdata.Surv24h_0_4$pmax <- pm.Surv24h_0_4[,3]
##Survival 36h 0-4h

# new dose levels as support for the line

newdata.Surv36h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv36h_0_4 <- predict(lm_36S_0_4, newdata = newdata.Surv36h_0_4, interval = "confidence")

# new data with predictions

newdata.Surv36h_0_4$p <- pm.Surv36h_0_4[,1]
newdata.Surv36h_0_4$pmin <- pm.Surv36h_0_4[,2]
newdata.Surv36h_0_4$pmax <- pm.Surv36h_0_4[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Surv48h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv48h_0_4 <- predict(lm_48S_0_4, newdata = newdata.Surv48h_0_4, interval = "confidence")

# new data with predictions

newdata.Surv48h_0_4$p <- pm.Surv48h_0_4[,1]
newdata.Surv48h_0_4$pmin <- pm.Surv48h_0_4[,2]
newdata.Surv48h_0_4$pmax <- pm.Surv48h_0_4[,3]

#Plot 48h

Inf_age_BMD$title04 <- "≤ 4 hpr"

plot_S1 <- ggplot() +
  geom_line(data = newdata.Surv24h_0_4, aes(x = tim, y = 100), color = "#DD8D29", size = 1) +
  geom_line(data = newdata.Surv36h_0_4, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Surv48h_0_4, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = 100, x = Conc, group = Conc), color = "#DD8D29",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_04_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_04_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Survival (% of control)") +
  theme_light() +
  facet_grid(. ~ title04) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))
  
plot_S1


###Predict curves for use with ggplot
##Survival 24h 4-8h

# new dose levels as support for the line

newdata.Surv24h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv24h_4_8 <- predict(lm_24S_4_8, newdata = newdata.Surv24h_4_8, interval = "confidence")

# new data with predictions

newdata.Surv24h_4_8$p <- pm.Surv24h_4_8[,1]
newdata.Surv24h_4_8$pmin <- pm.Surv24h_4_8[,2]
newdata.Surv24h_4_8$pmax <- pm.Surv24h_4_8[,3]
##Survival 36h 0-4h

# new dose levels as support for the line

newdata.Surv36h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv36h_4_8 <- predict(lm_36S_4_8, newdata = newdata.Surv36h_4_8, interval = "confidence")

# new data with predictions

newdata.Surv36h_4_8$p <- pm.Surv36h_4_8[,1]
newdata.Surv36h_4_8$pmin <- pm.Surv36h_4_8[,2]
newdata.Surv36h_4_8$pmax <- pm.Surv36h_4_8[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Surv48h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv48h_4_8 <- predict(lm_48S_4_8, newdata = newdata.Surv48h_4_8, interval = "confidence")

# new data with predictions

newdata.Surv48h_4_8$p <- pm.Surv48h_4_8[,1]
newdata.Surv48h_4_8$pmin <- pm.Surv48h_4_8[,2]
newdata.Surv48h_4_8$pmax <- pm.Surv48h_4_8[,3]

#Plot 48h

Inf_age_BMD$title48 <- "4-8 hpr"

plot_S2 <- ggplot() +
  geom_line(data = newdata.Surv24h_4_8, aes(x = tim, y = p), color = "#DD8D29", size = 1) +
  geom_line(data = newdata.Surv36h_4_8, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Surv48h_4_8, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_48_24, x = Conc, group = Conc), color = "#DD8D29",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_48_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_48_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Survival (% of control)") +
  theme_light() +
  facet_grid(. ~ title48) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks=c(0.5, 1, 2, 4, 8, 12)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_S2

###Predict curves for use with ggplot
##Survival 24h 8-12h

# new dose levels as support for the line

newdata.Surv24h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv24h_8_12 <- predict(lm_24S_8_12, newdata = newdata.Surv24h_8_12, interval = "confidence")

# new data with predictions

newdata.Surv24h_8_12$p <- pm.Surv24h_8_12[,1]
newdata.Surv24h_8_12$pmin <- pm.Surv24h_8_12[,2]
newdata.Surv24h_8_12$pmax <- pm.Surv24h_8_12[,3]
##Survival 36h 0-4h

# new dose levels as support for the line

newdata.Surv36h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv36h_8_12 <- predict(lm_36S_8_12, newdata = newdata.Surv36h_8_12, interval = "confidence")

# new data with predictions

newdata.Surv36h_8_12$p <- pm.Surv36h_8_12[,1]
newdata.Surv36h_8_12$pmin <- pm.Surv36h_8_12[,2]
newdata.Surv36h_8_12$pmax <- pm.Surv36h_8_12[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Surv48h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv48h_8_12 <- predict(lm_48S_8_12, newdata = newdata.Surv48h_8_12, interval = "confidence")

# new data with predictions

newdata.Surv48h_8_12$p <- pm.Surv48h_8_12[,1]
newdata.Surv48h_8_12$pmin <- pm.Surv48h_8_12[,2]
newdata.Surv48h_8_12$pmax <- pm.Surv48h_8_12[,3]

#Plot 48h

Inf_age_BMD$title812 <- "8-12 hpr"

plot_S3 <- ggplot() +
  geom_line(data = newdata.Surv24h_8_12, aes(x = tim, y = p, color = "24 h"), size = 1) +
  geom_line(data = newdata.Surv36h_8_12, aes(x = tim, y = p, color = "36 h"), size = 1) +
  geom_line(data = newdata.Surv48h_8_12, aes(x = tim, y = p, color = "48 h"), size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_812_24, x = Conc, group = Conc, color = "24 h"),
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_812_36, x = Conc, group = Conc, color = "36 h"),
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_812_48, x = Conc, group = Conc, color = "48 h"),
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Survival (% of control)", color = "Timepoint") +
  theme_light() +
  facet_grid(. ~ title812) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks=c(0.5, 1, 2, 4, 8, 12)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24),
        legend.title = element_text(size =20), 
        legend.text = element_text(size = 20)) +
  scale_colour_manual(values=c(wes_palette("FantasticFox1")[1], 
                               wes_palette("FantasticFox1")[3], 
                               wes_palette("FantasticFox1")[5])) 

plot_S3

###Predict curves for use with ggplot
##Survival 24h 0-12h

# new dose levels as support for the line

newdata.Surv24h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv24h_0_12 <- predict(lm_24S_0_12, newdata = newdata.Surv24h_0_12, interval = "confidence")

# new data with predictions

newdata.Surv24h_0_12$p <- pm.Surv24h_0_12[,1]
newdata.Surv24h_0_12$pmin <- pm.Surv24h_0_12[,2]
newdata.Surv24h_0_12$pmax <- pm.Surv24h_0_12[,3]
##Survival 36h 0-4h

# new dose levels as support for the line

newdata.Surv36h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv36h_0_12 <- predict(lm_36S_0_12, newdata = newdata.Surv36h_0_12, interval = "confidence")

# new data with predictions

newdata.Surv36h_0_12$p <- pm.Surv36h_0_12[,1]
newdata.Surv36h_0_12$pmin <- pm.Surv36h_0_12[,2]
newdata.Surv36h_0_12$pmax <- pm.Surv36h_0_12[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Surv48h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv48h_0_12 <- predict(lm_48S_0_12, newdata = newdata.Surv48h_0_12, interval = "confidence")

# new data with predictions

newdata.Surv48h_0_12$p <- pm.Surv48h_0_12[,1]
newdata.Surv48h_0_12$pmin <- pm.Surv48h_0_12[,2]
newdata.Surv48h_0_12$pmax <- pm.Surv48h_0_12[,3]

#Plot 48h

Inf_age_BMD$title012 <- "≤ 12 hpr"

plot_S4 <- ggplot() +
  geom_line(data = newdata.Surv24h_0_12, aes(x = tim, y = p), color = "#DD8D29", size = 1) +
  geom_line(data = newdata.Surv36h_0_12, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Surv48h_0_12, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_012_24, x = Conc, group = Conc), color = "#DD8D29",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_012_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_012_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Survival (% of control)") +
  theme_light() +
  facet_grid(. ~ title012) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks=c(0.5, 1, 2, 4, 8, 12)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_S4

###Predict curves for use with ggplot
##Survival 24h 0-24h

# new dose levels as support for the line

newdata.Surv24h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv24h_0_24 <- predict(lm_24S_0_24, newdata = newdata.Surv24h_0_24, interval = "confidence")

# new data with predictions

newdata.Surv24h_0_24$p <- pm.Surv24h_0_24[,1]
newdata.Surv24h_0_24$pmin <- pm.Surv24h_0_24[,2]
newdata.Surv24h_0_24$pmax <- pm.Surv24h_0_24[,3]
##Survival 36h 0-4h

# new dose levels as support for the line

newdata.Surv36h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv36h_0_24 <- predict(lm_36S_0_24, newdata = newdata.Surv36h_0_24, interval = "confidence")

# new data with predictions

newdata.Surv36h_0_24$p <- pm.Surv36h_0_24[,1]
newdata.Surv36h_0_24$pmin <- pm.Surv36h_0_24[,2]
newdata.Surv36h_0_24$pmax <- pm.Surv36h_0_24[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Surv48h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Surv48h_0_24 <- predict(lm_48S_0_24, newdata = newdata.Surv48h_0_24, interval = "confidence")

# new data with predictions

newdata.Surv48h_0_24$p <- pm.Surv48h_0_24[,1]
newdata.Surv48h_0_24$pmin <- pm.Surv48h_0_24[,2]
newdata.Surv48h_0_24$pmax <- pm.Surv48h_0_24[,3]

#Plot 48h

Inf_age_BMD$title024 <- "≤ 24 hpr"

cols <- c("24 h" = "#DD8D29", "36 h" = "#46ACC8", "48 h" = "#B40F20")
factors <- as.factor(c("24h", "36h", "48h"))

plot_S5 <- ggplot() +
  geom_line(data = newdata.Surv24h_0_24, aes(x = tim, y = p, color = "24 h"), size = 1) +
  geom_line(data = newdata.Surv36h_0_24, aes(x = tim, y = p, color = "36 h"), size = 1) +
  geom_line(data = newdata.Surv48h_0_24, aes(x = tim, y = p, color = "48 h"), size = 1) +
  stat_summary(data = Inf_age_BMD, show.legend = T,
               aes(y = S_024_24, x = Conc, group = Conc, color = "24 h"),
               fun = mean, 
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_024_36, x = Conc, group = Conc, color = "36 h"),
               fun = mean, 
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = S_024_48, x = Conc, group = Conc, color = "48 h"),
               fun = mean, 
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Survival (% of control)", color = "Timepoint") +
  theme_light() +
  facet_grid(. ~ title024) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(breaks =c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
  scale_colour_manual(values=c(wes_palette("FantasticFox1")[1], 
                      wes_palette("FantasticFox1")[3], 
                      wes_palette("FantasticFox1")[5])) 
  
plot_S5

###Predict curves for use with ggplot
##Molting 24h 0-4h

# new dose levels as support for the line

newdata.Molt24h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt24h_0_4 <- predict(lm_24M_0_4, newdata = newdata.Molt24h_0_4, interval = "confidence")

# new data with predictions

newdata.Molt24h_0_4$p <- pm.Molt24h_0_4[,1]
newdata.Molt24h_0_4$pmin <- pm.Molt24h_0_4[,2]
newdata.Molt24h_0_4$pmax <- pm.Molt24h_0_4[,3]
##Molting 36h 0-4h

# new dose levels as support for the line

newdata.Molt36h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt36h_0_4 <- predict(lm_36M_0_4, newdata = newdata.Molt36h_0_4, interval = "confidence")

# new data with predictions

newdata.Molt36h_0_4$p <- pm.Molt36h_0_4[,1]
newdata.Molt36h_0_4$pmin <- pm.Molt36h_0_4[,2]
newdata.Molt36h_0_4$pmax <- pm.Molt36h_0_4[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Molt48h_0_4 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt48h_0_4 <- predict(lm_48M_0_4, newdata = newdata.Molt48h_0_4, interval = "confidence")

# new data with predictions

newdata.Molt48h_0_4$p <- pm.Molt48h_0_4[,1]
newdata.Molt48h_0_4$pmin <- pm.Molt48h_0_4[,2]
newdata.Molt48h_0_4$pmax <- pm.Molt48h_0_4[,3]
#Plot 48h

Inf_age_BMD$title04 <- "≤ 4 hpr"

plot_M1 <- ggplot() +
  geom_line(data = newdata.Molt36h_0_4, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Molt48h_0_4, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_04_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_04_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Molting (% of control)") +
  theme_light() +
  facet_grid(. ~ title04) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(limits = c(-5, 120), breaks =c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_M1


###Predict curves for use with ggplot
##Molting 24h 0-4h

# new dose levels as support for the line

newdata.Molt24h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt24h_4_8 <- predict(lm_24M_4_8, newdata = newdata.Molt24h_4_8, interval = "confidence")

# new data with predictions

newdata.Molt24h_4_8$p <- pm.Molt24h_4_8[,1]
newdata.Molt24h_4_8$pmin <- pm.Molt24h_4_8[,2]
newdata.Molt24h_4_8$pmax <- pm.Molt24h_4_8[,3]
##Molting 36h 0-4h

# new dose levels as support for the line

newdata.Molt36h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt36h_4_8 <- predict(lm_36M_4_8, newdata = newdata.Molt36h_4_8, interval = "confidence")

# new data with predictions

newdata.Molt36h_4_8$p <- pm.Molt36h_4_8[,1]
newdata.Molt36h_4_8$pmin <- pm.Molt36h_4_8[,2]
newdata.Molt36h_4_8$pmax <- pm.Molt36h_4_8[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Molt48h_4_8 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt48h_4_8 <- predict(lm_48M_4_8, newdata = newdata.Molt48h_4_8, interval = "confidence")

# new data with predictions

newdata.Molt48h_4_8$p <- pm.Molt48h_4_8[,1]
newdata.Molt48h_4_8$pmin <- pm.Molt48h_4_8[,2]
newdata.Molt48h_4_8$pmax <- pm.Molt48h_4_8[,3]
#Plot 48h

Inf_age_BMD$title04 <- "4-8 hpr"

plot_M2 <- ggplot() +
  geom_line(data = newdata.Molt36h_4_8, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Molt48h_4_8, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_48_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_48_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Molting (% of control)") +
  theme_light() +
  facet_grid(. ~ title04) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(limits = c(-5, 120), breaks =c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_M2

###Predict curves for use with ggplot
##Molting 8-12 h

# new dose levels as support for the line

newdata.Molt24h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt24h_8_12 <- predict(lm_24M_8_12, newdata = newdata.Molt24h_8_12, interval = "confidence")

# new data with predictions

newdata.Molt24h_8_12$p <- pm.Molt24h_8_12[,1]
newdata.Molt24h_8_12$pmin <- pm.Molt24h_8_12[,2]
newdata.Molt24h_8_12$pmax <- pm.Molt24h_8_12[,3]
##Molting 36h 0-4h

# new dose levels as support for the line

newdata.Molt36h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt36h_8_12 <- predict(lm_36M_8_12, newdata = newdata.Molt36h_8_12, interval = "confidence")

# new data with predictions

newdata.Molt36h_8_12$p <- pm.Molt36h_8_12[,1]
newdata.Molt36h_8_12$pmin <- pm.Molt36h_8_12[,2]
newdata.Molt36h_8_12$pmax <- pm.Molt36h_8_12[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Molt48h_8_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt48h_8_12 <- predict(lm_48M_8_12, newdata = newdata.Molt48h_8_12, interval = "confidence")

# new data with predictions

newdata.Molt48h_8_12$p <- pm.Molt48h_8_12[,1]
newdata.Molt48h_8_12$pmin <- pm.Molt48h_8_12[,2]
newdata.Molt48h_8_12$pmax <- pm.Molt48h_8_12[,3]
#Plot 48h

Inf_age_BMD$title04 <- "8-12 hpr"

plot_M3 <- ggplot() +
  geom_line(data = newdata.Molt36h_8_12, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Molt48h_8_12, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_812_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_812_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Molting (% of control)") +
  theme_light() +
  facet_grid(. ~ title04) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(limits = c(-5, 120), breaks =c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_M3

###Predict curves for use with ggplot
##Molting 0-12 h

# new dose levels as support for the line

newdata.Molt24h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt24h_0_12 <- predict(lm_24M_0_12, newdata = newdata.Molt24h_0_12, interval = "confidence")

# new data with predictions

newdata.Molt24h_0_12$p <- pm.Molt24h_0_12[,1]
newdata.Molt24h_0_12$pmin <- pm.Molt24h_0_12[,2]
newdata.Molt24h_0_12$pmax <- pm.Molt24h_0_12[,3]
##Molting 36h 0-4h

# new dose levels as support for the line

newdata.Molt36h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt36h_0_12 <- predict(lm_36M_0_12, newdata = newdata.Molt36h_0_12, interval = "confidence")

# new data with predictions

newdata.Molt36h_0_12$p <- pm.Molt36h_0_12[,1]
newdata.Molt36h_0_12$pmin <- pm.Molt36h_0_12[,2]
newdata.Molt36h_0_12$pmax <- pm.Molt36h_0_12[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Molt48h_0_12 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt48h_0_12 <- predict(lm_48M_0_12, newdata = newdata.Molt48h_0_12, interval = "confidence")

# new data with predictions

newdata.Molt48h_0_12$p <- pm.Molt48h_0_12[,1]
newdata.Molt48h_0_12$pmin <- pm.Molt48h_0_12[,2]
newdata.Molt48h_0_12$pmax <- pm.Molt48h_0_12[,3]
#Plot 48h

Inf_age_BMD$title04 <- "≤ 12 hpr"

plot_M4 <- ggplot() +
  geom_line(data = newdata.Molt36h_0_12, aes(x = tim, y = p), color = "#46ACC8", size = 1) +
  geom_line(data = newdata.Molt48h_0_12, aes(x = tim, y = p), color = "#B40F20", size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_012_36, x = Conc, group = Conc), color = "#46ACC8",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_012_48, x = Conc, group = Conc), color = "#B40F20",
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Molting (% of control)") +
  theme_light() +
  facet_grid(. ~ title04) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(limits = c(-5, 120), breaks =c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24))

plot_M4

###Predict curves for use with ggplot
##Molting 0-12 h

# new dose levels as support for the line

newdata.Molt24h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt24h_0_24 <- predict(lm_24M_0_24, newdata = newdata.Molt24h_0_24, interval = "confidence")

# new data with predictions

newdata.Molt24h_0_24$p <- pm.Molt24h_0_24[,1]
newdata.Molt24h_0_24$pmin <- pm.Molt24h_0_24[,2]
newdata.Molt24h_0_24$pmax <- pm.Molt24h_0_24[,3]
##Molting 36h 0-4h

# new dose levels as support for the line

newdata.Molt36h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt36h_0_24 <- predict(lm_36M_0_24, newdata = newdata.Molt36h_0_24, interval = "confidence")

# new data with predictions

newdata.Molt36h_0_24$p <- pm.Molt36h_0_24[,1]
newdata.Molt36h_0_24$pmin <- pm.Molt36h_0_24[,2]
newdata.Molt36h_0_24$pmax <- pm.Molt36h_0_24[,3]
##Survival 48h 0-4h

# new dose levels as support for the line

newdata.Molt48h_0_24 <- expand.grid(tim = exp(seq(log(0.01), log(100), length = 100)))

# predictions and confidence intervals

pm.Molt48h_0_24 <- predict(lm_48M_0_24, newdata = newdata.Molt48h_0_24, interval = "confidence")

# new data with predictions

newdata.Molt48h_0_24$p <- pm.Molt48h_0_24[,1]
newdata.Molt48h_0_24$pmin <- pm.Molt48h_0_24[,2]
newdata.Molt48h_0_24$pmax <- pm.Molt48h_0_24[,3]
#Plot 48h

Inf_age_BMD$title04 <- "≤ 24 hpr"

plot_M5 <- ggplot() +
  geom_line(data = newdata.Molt36h_0_24, aes(x = tim, y = p, color = "36 h"), size = 1) +
  geom_line(data = newdata.Molt48h_0_24, aes(x = tim, y = p, color = "48 h"), size = 1) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_024_36, x = Conc, group = Conc, color = "36 h"),
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  stat_summary(data = Inf_age_BMD,
               aes(y = M_024_48, x = Conc, group = Conc, color = "48 h"),
               fun = mean,
               fun.min = function(y) mean(y) - sd(y) / sqrt(length(y)), 
               fun.max = function(y) mean(y) + sd(y) / sqrt(length(y)), 
               geom = "pointrange", size = 0.9) +
  labs(x = "Teflubenzuron (µg/L)", y = "Molting (% of control)", color = "Timepoint") +
  theme_light() +
  facet_grid(. ~ title024) +
  scale_x_continuous(limits = c(0.4, 13), trans = "log10", breaks = c(0.5, 1, 2, 4, 8, 12)) +
  scale_y_continuous(limits = c(-5, 120), breaks = c(0, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
  scale_colour_manual(values=c(wes_palette("FantasticFox1")[3], 
                               wes_palette("FantasticFox1")[5]))

plot_M5

plot_age_S <- ggarrange(plot_S1, plot_S2, plot_S3, common.legend = TRUE, nrow = 1, legend = "right")

plot_synch_S <- ggarrange(plot_S1, plot_S4, plot_S5, common.legend = TRUE, nrow = 1, legend = "right")

plot_agesynch_S <- ggarrange(plot_S1, plot_S2, plot_S3, plot_S4, plot_S5, common.legend = TRUE, legend = "right")

plot_agesynch_M <- ggarrange(plot_M1, plot_M2, plot_M3, plot_M4, plot_M5, common.legend = TRUE, legend = "right")

plot_agesynch_M

plot_age_S

plot_synch_S

plot_agesynch_S

ggsave(filename = "Plot_age_NETS.png", plot_age_S, width = 20, height = 6, dpi = 2000, path = "C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis")

ggsave(filename = "Plot_synch_NETS.png", plot_synch_S, width = 20, height = 6, dpi = 2000, path = "C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis")

ggsave(filename = "Plot_Surv_paper.png", plot_agesynch_S, width = 20, height = 10, dpi = 1000, path = "C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis")

ggsave(filename = "Plot_Molt_paper.png", plot_agesynch_M, width = 20, height = 10, dpi = 1000, path = "C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis")

LC50.data <- read_csv("C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis/BMD Analysis/Data/LC50.csv")

plot_LC50 <- ggplot(data = LC50.data) +
  geom_crossbar(aes(x = Age, y = LC50, ymin = Lower, ymax = Upper, fill = as.factor(Age))) +
  theme_light() +
  labs(y = "48h EC50 (µg/L)", fill = "Synchronization/Age", color = "Synchronization/Age") +
  scale_fill_manual(labels = c("≤ 4 hpr", "4-8 hpr", "8-12 hpr", "≤ 12 hpr", "≤ 24 hpr"), 
                      values=c(wes_palette("Cavalcanti1"))) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20))

plot_LC50

ggsave(filename = "Plot_LC50.png", plot_LC50, width = 20, height = 10, dpi = 1000, path = "C:/Users/SCH/OneDrive - NIVA/Work NIVA/Experiments/Influence of Age_Synch_april 2022/BMD Analysis")


