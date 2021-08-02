####################################################################################
##SEASONALITY OF FIRE IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA
##R code for plotting proportion of plot area burned vs. fuel moisture by treatment
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
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)

#import data from Excel worksheet#
#fire behavior plots
propburned_x_moist <- read_excel("input/propburned_vs_moist_fbp.xlsx", 
                                 col_types = c("text", "text", "text", "text", "text",
                                               "date", "numeric", "numeric", "numeric"))
propburned_x_moist <- data.frame(propburned_x_moist)

##plot proportion of plot area burned (aggregated by plot) by fuel moisture by treatment as scatterplot##
#pooled litter and 1-hr fuel moisture
plot.propburned.ltr1hr_moist <- ggplot(propburned_x_moist, aes(x = moist_avg_ltr_1hr, y = Prop_burn, color = Trt)) +
  scale_color_manual(propburned_x_moist$Trt, values = c("sienna4", "green4")) + #define series colors
  geom_point(size = 1.75) +
  geom_smooth(method = "lm", size = 0.75, fill = FALSE) + #create linear trendline
  labs(x = "Litter and 1-hr woody fuel moisture", y = "Plot area burned") + #write axis labels
  #set theme details
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        #set legend details
        legend.position = c(0.225, 0.8),
        legend.background = element_rect(fill = "gray90"),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 14),
        legend.key.width = unit(2, "line"),
        plot.margin = unit(c(0.075, 0.15, 0.075, 0.075), "in")) +
  scale_x_reverse(labels = percent) + #set continuous x-axis scale as percentage (reverse)
  scale_y_continuous(labels = percent, limits = c(0, 1)) + #set continuous y-axis scale as percentage
  #match series colors to legend series colors
  scale_color_manual(name = "Burn treatment",
                     breaks = c("d", "g"),
                     labels = c("Dormant season", "Growing season"),
                     values = c("sienna4", "green4"))
plot.propburned.ltr1hr_moist
#save to file
png("output/figures/ms 1/fig5_burned_moist_trt.png",
    width = 6.5,
    height = 4,
    units = "in",
    res = 600)
plot.propburned.ltr1hr_moist
dev.off()