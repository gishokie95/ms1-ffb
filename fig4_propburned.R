####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting proportion of plot area burned by treatment
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 07/02/20, R version 4.0.2
####################################################################################

##initial setup##

#set working directory
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/Fuels and Fire Behavior/Analysis/R")

#access libraries
library(readxl)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)

#import data from Excel worksheet
prop_burned<-read_excel("Data/propburned_p.xlsx", 
                        col_types=c("text","text","text","text","text","date","numeric"))
#convert to data frame
prop.burned<-data.frame(prop_burned)
#view imported data
View(prop.burned)

##plot proportion of plot area burned by treatment as boxplot##
plot.burned<-ggplot(prop.burned,aes(x=Trt,y=Prop_burn,color=Trt)) +
  scale_color_manual(prop.burned$Trt,values=c("sienna4","green4")) +
  geom_boxplot(size=2,outlier.size=2,width=0.625) +
  labs(x="Burn treatment",y="Plot area burned") +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(face="bold",color="black",size=16),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(face="bold",color="black",size=16),
        legend.position="none") +
  scale_x_discrete(labels=c("d"="Dormant season","g"="Growing season")) +
  scale_y_continuous(labels=percent)
plot.burned
#save to file
png("Figures/MS 1/fig4_prop_burned_x_trt.png",width=4.75,height=3.5,units="in",res=600)
plot.burned
dev.off()