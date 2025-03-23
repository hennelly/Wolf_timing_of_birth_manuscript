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


