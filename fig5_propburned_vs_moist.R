####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting proportion of plot area burned vs. in situ fuel moisture by treatment
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 04/13/20, R version 3.6.3
####################################################################################

##initial setup##

#set working directory
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/Fuels and Fire Behavior/Analysis/R")

#access libraries
library(readxl)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)

#import data from Excel worksheet
propburned_x_moist<-read_excel("Data/propburned_vs_moist_fbp.xlsx", 
                             col_types=c("text","text","text","text","text","date",
                                         "numeric","numeric","numeric"))
#convert to data frame
propburned_x_moist<-data.frame(propburned_x_moist)
#view imported data
View(propburned_x_moist)

##plot proportion of plot area burned by in situ fuel moisture by treatment,
##aggregated by plot, as scatterplot##

#pooled litter and 1-hr fuel moisture
plot.propburned.ltr1hr_moist<-ggplot(propburned_x_moist,aes(x=moist_avg_ltr_1hr,y=Prop_burn,color=Trt)) +
  scale_color_manual(propburned_x_moist$Trt,values=c("sienna4","green4")) +
  geom_point(size=1.75) +
  geom_smooth(method="lm",size=0.75,fill=FALSE) +
  labs(x="Litter and 1-hr woody fuel moisture",y="Plot area burned") +
  theme_classic() +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(face="bold",color="black",size=16),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(face="bold",color="black",size=16),
        legend.position=c(0.225,0.8),
        legend.background=element_rect(fill="gray90"),
        legend.title=element_text(color="black",size=16),
        legend.text=element_text(color="black",size=14),
        legend.key.width=unit(2,"line"),
        plot.margin=unit(c(0.075,0.15,0.075,0.075),"in")) +
  scale_x_reverse(labels=percent) +
  scale_y_continuous(labels=percent,limits=c(0,1)) +
  scale_color_manual(name="Burn treatment",
                     breaks=c("d","g"),
                     labels=c("Dormant season","Growing season"),
                     values=c("sienna4","green4"))
plot.propburned.ltr1hr_moist
#save to file
png("Figures/MS 1/fig5_propburned_x_moist_fbp_by_trt.png",width=6.5,height=4,units="in",res=600)
plot.propburned.ltr1hr_moist
dev.off()