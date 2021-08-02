####################################################################################
##SEASONALITY OF FIRE IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA
##R code for plotting time-integrated thermocouple probe heating by treatment
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

#import raw data from Excel worksheet#
#ap2g_06_16 removed
xabs60_fbgp <- read_excel("input/xabs60_fbgp_adj_nrrm.xlsx",
                          col_types = c("text", "text", "text", "text",
                                        "text", "text", "date", "numeric"))
xabs60_fbgp <- data.frame(xabs60_fbgp)

#aggregate data by plot#
xabs60_fbgp %>%
  group_by(DID, RID, Trt, UID, PID) %>%
  summarize(mean_XABS60_Cs = mean(XABS60_Cs)) %>%
  as.data.frame() -> xabs60_fbp

#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval
#be sure to load this function before running the rest of the code
xabs60_x_trt.mean <- summarySE(xabs60_fbp, measurevar = "mean_XABS60_Cs", groupvars = "Trt")

##plot time integral of temperature (xabs60) by treatment as means with standard error##
plot.xabs60 <- ggplot(xabs60_x_trt.mean, aes(x = Trt, y = mean_XABS60_Cs, color = Trt)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_XABS60_Cs - se, ymax = mean_XABS60_Cs + se), size = 1, width = 0.15) +
  scale_x_discrete(labels = c("d" = "Dormant season", "g" = "Growing season")) + #name treatments for x-axis categories
  scale_y_continuous(labels = function(x) sprintf("%.1f", x * 0.00001)) + #set continuous y-axis scale with scaled function
  scale_color_manual(xabs60_fbp$Trt, values = c("sienna4", "green4")) + #define series colors
  labs(x = "Burn treatment", y = "Thermocouple heating\n(100,000 °C s)") + #write axis labels
  #set theme details
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        legend.position = "none") #no legend
plot.xabs60
#save to file
png("output/figures/ms 1/fig6_xabs60_trt.png",
    width = 4.75,
    height = 3.5,
    units = "in",
    res = 600)
plot.xabs60
dev.off()