######################################################################
######################################################################
# Wild wolves: perform pairwise t-tests with Bonferroni's correction #
######################################################################
######################################################################
#we assessed whether wolf populations (North American, Iberian, Indian, SouthwestAsian, NorthernEuropean) were statistically different from each other in their timing of birth 

dat <- read.csv ("Feb23_PCAtemperature_GLMM.csv", header=TRUE)
pairwise.t.test(dat$DOY, dat$Region2, p.adjust.method="bonferroni")

##############################################################################
##############################################################################
# Wild vs. Captive wolves: statistical analyses and perform Welch's t-tests  #
##############################################################################
##############################################################################
# Here we tested whether the birth timing of wild and captive wolves statistically differed, using the entire wild and captive dataset

#read in dataset
dat <- read.csv("Feb25_comparison_wildvscaptive.csv", header=TRUE)

#Assess captive population
Captive <- subset(dat, Captive_vs_Wild=="Captive")
mean(Captive$adjusted_DOBirthYOD)
#184.142 May 3rd
sd(Captive$adjusted_DOBirthYOD)
#40.27963

#Assess wild population
Wild <- subset(dat, Captive_vs_Wild=="Wild")
mean(Wild$adjusted_DOBirthYOD)
#171.5325 April 21st
sd(Wild$adjusted_DOBirthYOD)
#42.13535

## Welch's t-test to compare wild and captive wolf timing of birth
dat <- read.csv("Feb25_WildvsCaptive_Ttest.csv", header=TRUE)
t.test(dat$Captive_adjusted_DOBirthYOD, dat$Wild_adjustedDOY, var.equal = FALSE) 
## Compare within population
dat <- read.csv("Captivewolves_DOY_Feb25_nodup.csv", header=TRUE)
pairwise.t.test(dat$adjusted_DOBirthYOD, dat$Scientific.Name, p.adjust.method="bonferroni")


####################################################################################
####################################################################################
# Wild vs. Captive wolves at same latitudes: perform t-tests with unequal variance #
####################################################################################
####################################################################################
# Here we tested whether the birth timing of wild and captive wolves statistically differed when comparing wolf indivdiuals from the same latitudinal range (by 10 degrees) 

#read in dataset
dat <- read.csv("Feb25_plotting_captivevswild_lat.csv", header=TRUE)

#testing difference at each 10 degrees
dat <- read.csv("Feb25_plotting_captivevswild_lat_ttest_10to20.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#pvalue-0.4715
dat <- read.csv("Feb25_plotting_captivevswild_lat_ttest_20to30.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#p-value = 0.83
dat <- read.csv("Feb25_plotting_captivevswild_lat_ttest_30to40.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#p-value = 0.1579
dat <- read.csv("Feb25_plotting_captivevswild_lat_40-50.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#p-value = 0.7259
dat <- read.csv("Feb25_plotting_captivevswild_lat_50-60.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#p-value = 8.837e-12
dat <- read.csv("Feb25_plotting_captivevswild_lat_60-70.csv", header=TRUE)
t.test(dat$Captive, dat$Wild, var.equal = FALSE) 
#p-value = 0.0001165





