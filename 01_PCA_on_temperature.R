###########################################
### PCA on temperature-related metrics ####
###########################################


install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library("factoextra")

#Read temperature-related variables 
dat <- read.csv ("PCA_Feb23_temperature.csv", header=TRUE)
data_normalized <- scale(dat) #normalize variables before doing PCA 

pairs(dat) #data correlated
pca <- prcomp(dat, scale. = TRUE) #fit PCA and scale that data. 
pca$rotation #rotation cofficients or eigenvectors. The coefficents describe how each PC is a linear combination of the input variables. 

#obtain eigenvectors
pca_scores <- data.frame(pca$x) #
head(pca_scores)
pairs(pca_scores) #data uncorrelated

write.csv (pca_scores, "PCA_scores_temperature.csv")
#Obtain eigenvalues
pca$sdev #how total variance is re-partitioned in derived principal components. 
var_exp <- data.frame(pc = 1:7,
                      var_exp = pca$sdev^2 / sum(pca$sdev^2))

# add the variances cumulatively
var_exp$var_exp_cumsum <- cumsum(var_exp$var_exp)
var_exp

#PC1 explains 65.97060 of the total variation. PC2 explains 22.4% of the total variation 

#plotting scree plot: 
par(mfrow=c(1,2))
plot(var_exp$pc, var_exp$var_exp, type="b", col=4,
     ylab="Fraction of explained variance", xlab="Principle Component")
plot(var_exp$pc, var_exp$var_exp_cumsum, type="b", col=6,
     ylab="Cummulative fraction variance", xlab="Principle Component")



#PC1 is driven by positive Annual_Mean_Temperature, Mean_Temperature_of all the quarters. And a negative relationship between temperature annual range. This corresponds to a high PC1 value is a seasonal cold environment vs. a low PC1 value being an aseasonal warm environment. 

# This means a high PC1 values corresponds to environments with high temperatures and little annual range, such as environmental closer to the equator with not much difference between summer and winter

#PC2 is driven by a negative relationship in mean diurnal range, negative relationship in temperature annual range, negative relationahip in mean temperature wettest quarter and mean temperature warmest quarter. 


