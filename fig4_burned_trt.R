####################################################################################
##SEASONALITY OF FIRE IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA
##R code for plotting proportion of plot area burned by treatment
##Joint Fire Science Program Project #16-1-06-12

##Matthew C. Vaughan, M.S. Graduate
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 08/01/21, R version 4.1.0
####################################################################################

##initial setup##

#set working directory#
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/analysis/R/treatment effects/fuels and fire behavior")

#access libraries#
library(readxl)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)

#import data from Excel worksheet#
prop_burned <- read_excel("input/propburned_p.xlsx", 
                          col_types = c("text", "text", "text", "text",
                                        "text", "date", "numeric"))
prop.burned <- data.frame(prop_burned)

##plot proportion of plot area burned by treatment as boxplot##
plot.burned <- ggplot(prop.burned, aes(x = Trt, y = Prop_burn, color = Trt)) +
  scale_color_manual(prop.burned$Trt, values = c("sienna4", "green4")) + #define x-axis category colors
  geom_boxplot(size = 2, outlier.size = 2, width = 0.625) +
  labs(x = "Burn treatment", y = "Plot area burned") + #write axis labels
  #set theme details
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        legend.position = "none") + #no legend
  scale_x_discrete(labels = c("d" = "Dormant season", "g" = "Growing season")) + #name treatments for x-axis categories
  scale_y_continuous(labels = percent) #set continuous y-axis scale as percentage
plot.burned
#save to file
png("output/figures/ms 1/fig4_burned_trt.png",
    width = 4.75,
    height = 3.5,
    units = "in",
    res = 600)
plot.burned
dev.off()