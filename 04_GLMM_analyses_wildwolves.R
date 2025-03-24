###################################################################################
###################################################################################
# GLMM analysis for wild wolves with only denID and PopulationID as random effect #
###################################################################################
###################################################################################

library(lme4) #for linear mixed models
library(MuMIn) #for AIC model selection

dat <- read.csv ("Feb23_PCAtemperature_GLMM.csv", header=TRUE)

#GLMM models and obtain AIC 
m1 <- lmer(DOY ~ day_change_rate_mating + (1|DenID) + (1| Region2), data = dat)
summary(m1)
AIC(m1)
#3195.231
m2 <- lmer(DOY ~ Annual_Precipitation + (1|DenID) + (1| Region2), data = dat)
summary(m2)
AIC(m2)
#3354.294
m3 <- lmer(DOY ~ Precipitation_Seasonality + (1|DenID) + (1| Region2), data = dat)
summary(m3)
AIC(m3)
#3346.999
m4 <- lmer(DOY ~ PC1 + (1|DenID)  + (1| Region2), data = dat)
summary(m4)
AIC(m4)
#3304.188
m5 <- lmer(DOY ~ PC1 + Annual_Precipitation + Precipitation_Seasonality + (1|DenID) + (1| Region2), data = dat)
summary(m5)
AIC(m5)
#3315.185
m6 <- lmer(DOY ~ day_change_rate_mating + Annual_Precipitation + Precipitation_Seasonality + (1|DenID) + (1| Region2), data = dat)
summary(m6)
AIC(m6)
#3200.938
m7 <- lmer(DOY ~ PC1 + Precipitation_Seasonality + (1|DenID) + (1|Region2), data = dat)
summary(m7)
AIC(m7)
#3309.423
m8 <- lmer(DOY ~ day_change_rate_mating + Precipitation_Seasonality + (1|DenID) + (1| Region2), data = dat)
summary(m8)
AIC(m8)
#3190.847
m9 <- lmer(DOY ~ PC1 + Annual_Precipitation + (1|DenID) + (1| Region), data = dat)
summary(m9)
AIC(m9)
#3338.025 
m10 <- lmer(DOY ~ day_change_rate_mating + Annual_Precipitation + (1|DenID) + (1| Region2) , data = dat)
summary(m10)
AIC(m10)
#3201.856
m11 <- lmer(DOY ~ Precipitation_Seasonality + Annual_Precipitation + (1|DenID) + (1| Region2), data = dat)
summary(m11)
AIC(m11)
#3358.659


##################################################################
##################################################################
# GLMM analysis for wild wolves with only denID as random effect #
##################################################################
##################################################################
library(lme4) #for linear mixed models
library(MuMIn) #for AIC model selection

dat <- read.csv ("Feb23_PCAtemperature_GLMM.csv", header=TRUE)

#GLMM models and obtain AIC 
m1 <- lmer(DOY ~ day_change_rate_mating + (1|DenID), data = dat)
summary(m1)
AIC(m1)
#3501.491
m2 <- lmer(DOY ~ Annual_Precipitation + (1|DenID), data = dat)
summary(m2)
AIC(m2)
#3728.668
m3 <- lmer(DOY ~ Precipitation_Seasonality + (1|DenID), data = dat)
summary(m3)
AIC(m3)
#3645.592
m4 <- lmer(DOY ~ PC1 + (1|DenID)  + (1| Region), data = dat)
summary(m4)
AIC(m4)
#3561.238
m5 <- lmer(DOY ~ PC1 + Annual_Precipitation + Precipitation_Seasonality + (1|DenID), data = dat)
summary(m5)
AIC(m5)
#3536.972
m6 <- lmer(DOY ~ day_change_rate_mating + Annual_Precipitation + Precipitation_Seasonality + (1|DenID), data = dat)
summary(m6)
AIC(m6)
#3486.533
m7 <- lmer(DOY ~ PC1 + Precipitation_Seasonality + (1|DenID) + (1|Region), data = dat)
summary(m7)
AIC(m7)
#3527.628
m8 <- lmer(DOY ~ day_change_rate_mating + Precipitation_Seasonality + (1|DenID), data = dat)
summary(m8)
AIC(m8)
#3475.886
m9 <- lmer(DOY ~ PC1 + Annual_Precipitation + (1|DenID), data = dat)
summary(m9)
AIC(m9)
#3553.005 
m10 <- lmer(DOY ~ day_change_rate_mating + Annual_Precipitation + (1|DenID), data = dat)
summary(m10)
AIC(m10)
#3501.2
m11 <- lmer(DOY ~ Precipitation_Seasonality + Annual_Precipitation + (1|DenID), data = dat)
summary(m11)
AIC(m11)
#3553.005


