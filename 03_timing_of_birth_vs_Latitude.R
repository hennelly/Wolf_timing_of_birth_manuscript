##################################################
##################################################
## Latitude vs. timing of birth for wild wolves ##
##################################################
##################################################
library(lme4)
library(MuMIn)

#read dataset
dat <- read.csv ("cleaned_wolf_parturition_withIran_Feb16_2025_transformed.csv", header=TRUE)

#linear mixed model using DenID as a random effect
mixed.lmer <- lmer(DOY ~ Lat + (1|DenID) , data = dat)
summary(mixed.lmer)
#Obtain variances for the residual and random efefct. 
#Fixed effects:
#            Estimate Std. Error t value
#(Intercept)  83.3004     6.3459   13.13
#Lat           1.7576     0.1193   14.73

fixef(mixed.lmer)
#fixed-effect parameters
#(Intercept)         Lat 
#  83.300395    1.757599 

#This means that there is a positive relationship between latitude and DOY. Every 1 step increase in DOY, there is a 1.75 increase in Lat. 

confint(mixed.lmer) # confident range 

# To determine the percentage of variance explained by the fixed effect, we use the marginal R2, which represents the proportion of variance in the dependent variable that is explained by the fixed effect alone. 
# Marginal R2 - measures the variance explained by the fixed effects only, without accounting for random effects

r_squared <- r.squaredGLMM(mixed.lmer)
print(r_squared)

#R2m       R2c
#[1,] 0.4528089 0.9099828


#######################
# For European wolves  #
########################
dat <- read.csv("cleaned_wolf_parturition_withIran_Feb16_2025_transformed.csv", header=TRUE)

Europe <- subset(dat, Region=="Europe")

#Use DenID for random effect

mixed.lmer <- lmer(DOY ~ Lat + (1|DenID) , data = Europe)
summary(mixed.lmer)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: DOY ~ Lat + (1 | DenID)
#   Data: Europe

#REML criterion at convergence: 425.6

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-1.97486 -0.37301 -0.01575  0.44881  1.32218 

#Random effects:
# Groups   Name        Variance Std.Dev.
# DenID    (Intercept) 39.60    6.293   
# Residual             15.04    3.878   
#Number of obs: 63, groups:  DenID, 58

#Fixed effects:
#            Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept) 241.7267     6.0431  58.1701   40.00  < 2e-16 ***
#Lat          -0.9611     0.1159  58.1440   -8.29 1.97e-11 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#    (Intr)
#Lat -0.987
#         1.7576     0.1193   14.73


#####################################################
#####################################################
## Latitude vs. timing of birth for captive wolves ##
#####################################################
#####################################################
#read dataset
dat <- read.csv("Feb25_plotting_captivevswild_lat.csv", header=TRUE)

Captive <- subset(dat, Group=="captive")

mixed.lmer <- lm(DOY ~ Lat  , data = Captive)
summary(mixed.lmer)

#Call:
#lm(formula = DOY ~ Lat, data = Captive)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-184.18  -10.85    0.12   12.78  193.90 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 100.6979     5.5317   18.20   <2e-16 ***
#Lat           1.6796     0.1079   15.57   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 34.3 on 666 degrees of freedom
Multiple R-squared:  0.2669,	Adjusted R-squared:  0.2658 
F-statistic: 242.5 on 1 and 666 DF,  p-value: < 2.2e-16





