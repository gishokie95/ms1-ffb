####################################################################################
##SEASONALITY OF FIRE IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA
##R code for plotting bole char height vs. topographic GIS variables by unit and treatment
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

#import data from Excel worksheets#
#bole char height (all plots)
charht_x_gis <- read_excel("input/charht0s_vs_gis_p.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "text", "numeric", "text", "date",
                                         "numeric", "numeric", "numeric", "numeric"))
charht_x_gis <- data.frame(charht_x_gis)

#transform variables as needed#
charht_x_gis$ht_char0s_m <- charht_x_gis$ht_char0s_cm * 0.01

##plot char height by landscape GIS variables by treatment, aggregated by plot, as scatterplot##

#Dormant season#
#TPI
plot.charht.tpi.ds <- ggplot(subset(charht_x_gis, Trt %in% "d"),
                             aes(x = tpi_avg_p_nrm, y = ht_char0s_m, color = UID)) +
  geom_point(size = 1.75) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              size = 0.75) +
  geom_smooth(mapping = aes(color = Trt),
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              linetype = "dashed",
              size = 1.25) +
  scale_color_manual(values = c("black", "tan4", "tan2", "gray50", "sienna4")) + #define series colors
  #set theme details
  theme_classic() +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_blank(),
        legend.position = "none") + #no legend
  scale_x_continuous(limits = c(0.25, 0.8), breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) + #set continuous x-axis scale with defined limits and breaks
  scale_y_continuous(limits = c(0, 2.75), breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) #set continuous y-axis scale with defined limits and breaks
#HLI
plot.charht.hli.ds <- ggplot(subset(charht_x_gis, Trt %in% "d"),
                             aes(x = hli_avg_p_nrm, y = ht_char0s_m, color = UID)) +
  geom_point(size = 1.75) +
  geom_smooth(mapping = aes(linetype = UID, size = UID),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  geom_smooth(mapping = aes(color = Trt, linetype = Trt, size = Trt),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  #define series colors
  scale_color_manual(name = "DS burns",
                     breaks = c("ap1d", "ap2d", "cr2d", "cr3d", "d"),
                     labels = c("AP1D", "AP2D", "CR2D", "CR3D", "All"),
                     values = c("black", "tan4", "tan2", "gray50", "sienna4")) +
  #define series line types
  scale_linetype_manual(name = "DS burns",
                        breaks = c("ap1d", "ap2d", "cr2d", "cr3d", "d"),
                        labels = c("AP1D", "AP2D", "CR2D", "CR3D", "All"),
                        values = c("solid", "solid", "solid", "solid", "dashed")) +
  #define series sizes
  scale_size_manual(name = "DS burns",
                    breaks = c("ap1d", "ap2d", "cr2d", "cr3d", "d"),
                    labels = c("AP1D", "AP2D", "CR2D", "CR3D", "All"),
                    values = c(0.75, 0.75, 0.75, 0.75, 1.25)) +
  #set theme details
  theme_classic() +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_blank(),
        #set legend details
        legend.position = c(0.3, 0.7),
        legend.background = element_rect(fill = "gray90"),
        legend.title = element_text(color = "black", size = 11),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.margin = margin(2, 2, 2, 2)) +
  scale_x_continuous(limits = c(0.45, 0.825), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) + #set continuous x-axis scale with defined limits and breaks
  scale_y_continuous(limits = c(0, 2.75), breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) #set continuous y-axis scale with defined limits and breaks

#Growing season#
#TPI
plot.charht.tpi.gs <- ggplot(subset(charht_x_gis, Trt %in% "g"),
                             aes(x = tpi_avg_p_nrm, y = ht_char0s_m, color = UID)) +
  geom_point(size = 1.75) +
  geom_smooth(mapping = aes(linetype = UID, size = UID),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  geom_smooth(mapping = aes(color = Trt, linetype = Trt, size = Trt),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  #define series colors
  scale_color_manual(name = "GS burns",
                     breaks = c("ap1g", "ap2g", "cr2g", "g"),
                     labels = c("AP1G", "AP2G", "CR2G", "All"),
                     values = c("darkgreen", "seagreen3", "lawngreen", "green4")) +
  #define series line types
  scale_linetype_manual(name = "GS burns",
                        breaks = c("ap1g", "ap2g", "cr2g", "g"),
                        labels = c("AP1G", "AP2G", "CR2G", "All"),
                        values = c("solid", "solid", "solid", "dashed")) +
  #define series sizes
  scale_size_manual(name = "GS burns",
                    breaks = c("ap1g", "ap2g", "cr2g", "g"),
                    labels = c("AP1G", "AP2G", "CR2G", "All"),
                    values = c(0.75, 0.75, 0.75, 1.25)) +
  #set theme details
  theme_classic() +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #set legend details
        legend.position = c(0.825, 0.75),
        legend.background = element_rect(fill = "gray90"),
        legend.title = element_text(color = "black", size = 11),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.margin = margin(2, 2, 2, 2)) +
  scale_x_continuous(limits = c(0.25, 0.8), breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) + #set continuous x-axis scale with defined limits and breaks
  scale_y_continuous(limits = c(0, 2.75), breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) #set continuous y-axis scale with defined limits and breaks
#HLI
plot.charht.hli.gs <- ggplot(subset(charht_x_gis, Trt %in% "g"),
                             aes(x = hli_avg_p_nrm, y = ht_char0s_m, color = UID)) +
  geom_point(size = 1.75) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              size = 0.75) +
  geom_smooth(mapping = aes(color = Trt),
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              linetype = "dashed",
              size = 1.25) +
  #define series colors
  scale_color_manual(values = c("darkgreen", "seagreen3", "lawngreen", "green4")) +
  #set theme details
  theme_classic() +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") + #no legend
  scale_x_continuous(limits = c(0.45, 0.825), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) + #set continuous x-axis scale with defined limits and breaks
  scale_y_continuous(limits = c(0, 2.75), breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) #set continuous y-axis scale with defined limits and breaks

#arrange plots into multi-panel figure#
figure.charht.tpi <- ggarrange(plot.charht.tpi.ds, plot.charht.tpi.gs,
                               ncol = 2, nrow = 1, align = "hv",
                               labels = c("a", "b"),
                               font.label = list(size = 14),
                               hjust = -4,
                               vjust = 2)
#write x-axis label for TPI sub-figures for multi-panel figure as annotation
figure.charht.tpi <- annotate_figure(figure.charht.tpi,
                                     bottom = text_grob("Topographic Position Index (TPI)",
                                     face = "bold",
                                     color = "black",
                                     size = 15))
figure.charht.hli <- ggarrange(plot.charht.hli.ds, plot.charht.hli.gs,
                               ncol = 2, nrow = 1, align = "hv",
                               labels = c("c", "d"),
                               font.label = list(size = 14),
                               hjust = -4,
                               vjust = 2)
#write x-axis label for HLI sub-figures for multi-panel figure as annotation
figure.charht.hli <- annotate_figure(figure.charht.hli,
                                     bottom = text_grob("Heat Load Index (HLI)",
                                     face = "bold",
                                     color = "black",
                                     size = 15))
figure.charht <- ggarrange(figure.charht.tpi, figure.charht.hli, ncol = 1, nrow = 2)
#write y-axis label for multi-panel figure as annotation
figure.charht <- annotate_figure(figure.charht,
                                 left = text_grob("Mean bole char height (m)",
                                 face = "bold",
                                 color = "black",
                                 size = 15,
                                 rot = 90))
figure.charht
#save to file
png("output/figures/ms 1/fig8_bch_tpi+hli_unit_trt.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 600)
figure.charht
dev.off()