###############################
###############################
## Code for plotting figures ##
###############################
###############################
#written by Lauren Hennelly

############################################################
# Figure 1A - Geographic map of wild wolf timing of births #
############################################################
library(ggplot2)
library(maps)
library(mapdata)
library(rworldmap)

dat <- read.csv ("cleaned_wolf_parturition_withIran_Feb16_2025_transformed_FINAL.csv", header=TRUE)

world <- map_data('world')
p <- ggplot(world, aes(lat, long)) +
    geom_map(map=world, aes(map_id=region), fill="gray95", color="darkgray") +
    coord_quickmap() + theme_classic() 
 
q <- p +  geom_point(data = dat, 
             aes(x = Long, y = Lat, fill=DOY), 
             alpha = 1, 
             size = 2.5, shape = 21) +
   scale_fill_gradient2(low = "white", mid="yellow", high = "blue", midpoint = 125, na.value = NA) 
q + scale_y_continuous(breaks = round(seq(min(10), max(90), by = 10),0))
ggsave("Feb25_Figure1.pdf", width=20,height=11)


#############################################################
### Figure 1B - box plot of timing of birth by latitude  ####
#############################################################
dat <- read.csv("Published_unpublished_DOY_vs_latitude_Mar82025_FINAL2.csv", header=TRUE)

dat <- read.csv("March8_plotting_captivevswild_lat.csv", header=TRUE)

Wild <- subset(dat, Group=="wild ")

p1=Wild %>% ggplot(aes(x=LatGrouping_by10, y=DOY))+  
  geom_boxplot()+
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0), 
             pch=21, size=2.5, aes(fill=factor(Region2)))
p2 <- p1+scale_fill_manual(values=c("olivedrab3", "forestgreen", "orangered", "darkgoldenrod1", "#56B4E9", "gray", "purple", "gray", "white", "black", "brown")) + theme_classic()
p2 + scale_y_continuous(breaks = round(seq(min(25), max(250), by = 25),0)) 

ggsave("March8_Figure1b.pdf", width=6,height=4)


############################################################################################
### Figure 1C - Scatterplot of timing of birth by latitude including literature sources ####
############################################################################################
dat <- read.csv("Published_unpublished_DOY_vs_latitude_Mar82025_FINAL2.csv", header=TRUE)

plot <- ggplot(dat, aes(x=DOY, y=Lat,
                                fill = Region2))+ 
          geom_point(size=3, shape=dat$unpublished.vs.published2) + 
  geom_errorbar(aes(ymin = Lat_lowerrange, ymax = Lat_upperrange, color=Region2)) +
  geom_errorbarh(aes(xmin = DOY_lowerrange, xmax = DOY_upperrange, color=Region2)) +
        theme_classic() +scale_fill_manual(values=c("purple", "olivedrab3", "forestgreen", "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray")) +scale_color_manual(values=c( "purple", "olivedrab3" , "forestgreen", "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray"))  

plot3 <- plot + geom_smooth(aes(group = "Region"), method="lm", se=FALSE, col="black") 
plot3 + scale_y_continuous(breaks = round(seq(min(10), max(90), by = 10),0)) + scale_x_continuous(breaks = round(seq(min(0), max(250), by = 25),0)) 

ggsave("March8_Figure1c.pdf", width=7,height=4)

##########################################################################
### Figure 3A - Boxplot of captive wolf  timing of birth by latitude  ####
##########################################################################
dat <- read.csv("March8_plotting_captivevswild_lat.csv", header=TRUE)

Captive <- subset(dat, Group=="captive")


p1=Captive %>% ggplot(aes(x=LatGrouping_by10, y=DOY))+  
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0), 
             pch=21, size=2.5, aes(fill=factor(Region2)))
p2 <- p1+scale_fill_manual(values=c("darkgoldenrod1", "olivedrab3", "orangered", "#56B4E9", "#56B4E9", "gray", "purple", "gray", "white", "black", "brown")) + theme_classic()
p2 + scale_y_continuous(breaks = round(seq(min(25), max(365), by = 25),0)) 

ggsave("March13_Figure1A.pdf", width=10,height=4)

###################################################################################################
### Figure 3B - Scatterplot of captive wolf vs wild wolf timing of birth with regression line  ####
###################################################################################################
dat <- read.csv("March8_plotting_captivevswild_lat.csv", header=TRUE)

plot <- ggplot(dat, aes(x=DOY, y=Lat ,color=Group)) + 
          geom_point(size=3, aes(fill=Group), colour="black",pch=21) + geom_smooth(method=lm) + theme_classic() +scale_color_manual(values=c("lightgoldenrod", "honeydew4")) + scale_fill_manual(values=c("lightgoldenrod", "honeydew4") )

plot + scale_y_continuous(breaks = round(seq(min(10), max(90), by = 10),0)) + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25),0)) + stat_poly_line() +     stat_poly_eq()

ggsave("March22_Figure3B.pdf", width=10,height=4)

###########################################################################
### Figure 3C - Boxplot of captive wolf vs wild wolf timing of birth   ####
###########################################################################
dat <- read.csv("March8_plotting_captivevswild_lat.csv", header=TRUE)

plot <- ggplot(dat, aes(x=LatGrouping_by10, y=DOY, colour=Group, fill=Group)) +
  geom_boxplot(position = position_dodge(width = 1), fill = NA, lwd=1)


p2 <- plot+scale_color_manual(values=c("lightgoldenrod", "honeydew4")) + scale_fill_manual(values=c("black", "black")) + theme_classic()

p2 + scale_y_continuous(breaks = round(seq(min(25), max(365), by = 25),0)) 

ggsave("March22_Figure3c.pdf", width=10,height=3)

#############################################################
### Figure S1 - Daylength change over a year by latitude ####
#############################################################

# Load required libraries
library(ggplot2)

# Function to calculate daylength in hours
daylength <- function(latitude, day_of_year) {
  # Convert latitude to radians
  lat_rad <- latitude * pi / 180
  
  # Solar declination angle (approximation)
  declination <- 23.44 * pi / 180 * sin(2 * pi * (day_of_year - 81) / 365)
  
  # Calculate hour angle
  cos_omega <- -tan(lat_rad) * tan(declination)
  
  # Limit values to avoid errors due to numerical precision
  cos_omega <- pmax(pmin(cos_omega, 1), -1)
  
  # Hour angle in radians
  omega <- acos(cos_omega)
  
  # Daylength in hours
  return(24 * omega / pi)
}

# Define latitude - do this for all latitudes I want to plot and save as a csv
latitude <- 40 

# Create a data frame for plotting
days <- 1:365
daylengths <- sapply(days, function(d) daylength(latitude, d))
data <- data.frame(Day = days, Daylength = daylengths)

write.csv(data, "daylength_40latitude.csv")

# Plot the daylength over a year
library(RColorBrewer)

dat <- read.csv("March7_daylength_latitude_Figure_Final.csv", header=TRUE)
                     
p <- ggplot(dat, aes(x = Day, y = Daylength, color=Latitude)) +
  geom_line( size = 1) +
  labs( x = "Day of the Year",
       y = "Daylength (hours)") +
  theme_classic()
p + scale_color_brewer(palette = "RdYlBu") + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25),0)) + scale_y_continuous(breaks = round(seq(min(0), max(25), by = 5),0))

ggsave("March7_FigureS1.pdf", width=10,height=6)


