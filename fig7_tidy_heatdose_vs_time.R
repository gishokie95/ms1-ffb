####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting time series of time-integrated thermocouple probe heating (refined)
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 04/22/20, R version 3.6.3
####################################################################################

##initial setup##

#set working directory#
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/Fuels and Fire Behavior/Analysis/R")

#access libraries#
library(readxl)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(tidyquant)
library(tidyr)
library(naniar)
library(zoo)
library(data.table)

#import raw data from Excel worksheet#
#thermocouple temperature recordings for values >= 60 °C
#grid points ran in MATLAB
tcp_ABS60<-read_excel("Data/tcp_temp_abs60_ssm_adj_nrrm.xlsx", 
                      col_types=c("numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                  "numeric"))

#convert imported data to data frame#
tcp_ABS60<-data.frame(tcp_ABS60)

#convert data frame to tidy structure#
tcp_ABS60_tidy<-gather(tcp_ABS60,GPID,temp,-time_sm_dstadj_s)
#add location information (DID, RID, Trt, UID, PID) to new fields
tcp_ABS60_tidy$DID<-substr(tcp_ABS60_tidy$GPID,start=1,stop=2)
tcp_ABS60_tidy$RID<-substr(tcp_ABS60_tidy$GPID,start=1,stop=3)
tcp_ABS60_tidy$Trt<-substr(tcp_ABS60_tidy$GPID,start=4,stop=4)
tcp_ABS60_tidy$UID<-substr(tcp_ABS60_tidy$GPID,start=1,stop=4)
tcp_ABS60_tidy$PID<-substr(tcp_ABS60_tidy$GPID,start=1,stop=7)
#re-arrange order of fields
tcp_ABS60_tidy<-tcp_ABS60_tidy[,c(1,4,5,6,7,8,2,3)]
#preview tidy data
head(tcp_ABS60_tidy)


##calculate averages for individual burns (x4 d, x3 g)##

#dormant season#

#AP1D Russell Mountain#
#filter data
ap1d_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "ap1d")
#instantaneous
ap1d_ABS60_inst_avg=tapply(ap1d_ABS60_tidy$temp,ap1d_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
ap1d_ABS60_1hroll_avg=frollmean(ap1d_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
ap1d_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,ap1d_ABS60_inst_avg,ap1d_ABS60_1hroll_avg)

#AP2D Joels Ridge#
#filter data
ap2d_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "ap2d")
#instantaneous
ap2d_ABS60_inst_avg=tapply(ap2d_ABS60_tidy$temp,ap2d_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
ap2d_ABS60_1hroll_avg=frollmean(ap2d_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
ap2d_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,ap2d_ABS60_inst_avg,ap2d_ABS60_1hroll_avg)

#CR2D Big Ridge#
#filter data
cr2d_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "cr2d")
#instantaneous
cr2d_ABS60_inst_avg=tapply(cr2d_ABS60_tidy$temp,cr2d_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
cr2d_ABS60_1hroll_avg=frollmean(cr2d_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
cr2d_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,cr2d_ABS60_inst_avg,cr2d_ABS60_1hroll_avg)

#CR3D Farmer Mountain#
#filter data
cr3d_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "cr3d")
#instantaneous
cr3d_ABS60_inst_avg=tapply(cr3d_ABS60_tidy$temp,cr3d_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
cr3d_ABS60_1hroll_avg=frollmean(cr3d_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
cr3d_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,cr3d_ABS60_inst_avg,cr3d_ABS60_1hroll_avg)

#growing season#

#AP1G Moss Mill#
#filter data
ap1g_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "ap1g")
#instantaneous
ap1g_ABS60_inst_avg=tapply(ap1g_ABS60_tidy$temp,ap1g_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
ap1g_ABS60_1hroll_avg=frollmean(ap1g_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
ap1g_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,ap1g_ABS60_inst_avg,ap1g_ABS60_1hroll_avg)

#AP2G Drummond Creek#
#filter data
ap2g_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "ap2g")
#instantaneous
ap2g_ABS60_inst_avg=tapply(ap2g_ABS60_tidy$temp,ap2g_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
ap2g_ABS60_1hroll_avg=frollmean(ap2g_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
ap2g_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,ap2g_ABS60_inst_avg,ap2g_ABS60_1hroll_avg)

#CR2G Ducks Nest Gap#
#filter data
cr2g_ABS60_tidy<-subset(tcp_ABS60_tidy,UID %in% "cr2g")
#instantaneous
cr2g_ABS60_inst_avg=tapply(cr2g_ABS60_tidy$temp,cr2g_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling
cr2g_ABS60_1hroll_avg=frollmean(cr2g_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#bind variables to data frame
cr2g_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,cr2g_ABS60_inst_avg,cr2g_ABS60_1hroll_avg)


##calculate metrics for treatments (d, g)##

#dormant season#
#filter data
d_ABS60_tidy<-subset(tcp_ABS60_tidy,Trt %in% "d")
#sample size (n)
#instantaneous
d_ABS60_inst_n=tapply(d_ABS60_tidy$temp,d_ABS60_tidy$time_sm_dstadj_s,function(x) length(which(!is.na(x))))
#1 hr rolling mean
d_ABS60_1hroll_n=frollmean(d_ABS60_inst_n,3600,align="center",na.rm=TRUE)
#mean (avg)
#instantaneous
d_ABS60_inst_avg=tapply(d_ABS60_tidy$temp,d_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling mean
d_ABS60_1hroll_avg=frollmean(d_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#standard deviation (sd)
#instantaneous
d_ABS60_inst_sd=tapply(d_ABS60_tidy$temp,d_ABS60_tidy$time_sm_dstadj_s,sd,na.rm=TRUE)
#1 hr rolling mean
d_ABS60_1hroll_sd=frollmean(d_ABS60_inst_sd,3600,align="center",na.rm=TRUE)
#standard error of the mean (sem)
#instantaneous
d_ABS60_inst_sem=d_ABS60_inst_sd/sqrt(d_ABS60_inst_n)
#1 hr rolling mean
d_ABS60_1hroll_sem=frollmean(d_ABS60_inst_sem,3600,align="center",na.rm=TRUE)
#95% confidence interval (ci)
#upper bound
#instantaneous
d_ABS60_inst_ci_lower<-d_ABS60_inst_avg+qt((1-0.95)/2,df=d_ABS60_inst_n-1)*d_ABS60_inst_sem
#1 hr rolling mean
d_ABS60_1hroll_ci_lower=frollmean(d_ABS60_inst_ci_lower,3600,align="center",na.rm=TRUE)
#lower bound
#instantaneous
d_ABS60_inst_ci_upper<-d_ABS60_inst_avg-qt((1-0.95)/2,df=d_ABS60_inst_n-1)*d_ABS60_inst_sem
#1 hr rolling mean
d_ABS60_1hroll_ci_upper=frollmean(d_ABS60_inst_ci_upper,3600,align="center",na.rm=TRUE)
#bind metrics to data frame
d_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,
                        d_ABS60_inst_n,d_ABS60_1hroll_n,
                        d_ABS60_inst_avg,d_ABS60_1hroll_avg,
                        d_ABS60_inst_sd,d_ABS60_1hroll_sd,
                        d_ABS60_inst_sem,d_ABS60_1hroll_sem,
                        d_ABS60_inst_ci_lower,d_ABS60_1hroll_ci_lower,
                        d_ABS60_inst_ci_upper,d_ABS60_1hroll_ci_upper)

#growing season#
#filter data
g_ABS60_tidy<-subset(tcp_ABS60_tidy,Trt %in% "g")
#sample size (n)
#instantaneous
g_ABS60_inst_n=tapply(g_ABS60_tidy$temp,g_ABS60_tidy$time_sm_dstadj_s,function(x) length(which(!is.na(x))))
#1 hr rolling mean
g_ABS60_1hroll_n=frollmean(g_ABS60_inst_n,3600,align="center",na.rm=TRUE)
#mean (avg)
#instantaneous
g_ABS60_inst_avg=tapply(g_ABS60_tidy$temp,g_ABS60_tidy$time_sm_dstadj_s,mean,na.rm=TRUE)
#1 hr rolling mean
g_ABS60_1hroll_avg=frollmean(g_ABS60_inst_avg,3600,align="center",na.rm=TRUE)
#standard deviation (sd)
#instantaneous
g_ABS60_inst_sd=tapply(g_ABS60_tidy$temp,g_ABS60_tidy$time_sm_dstadj_s,sd,na.rm=TRUE)
#1 hr rolling mean
g_ABS60_1hroll_sd=frollmean(g_ABS60_inst_sd,3600,align="center",na.rm=TRUE)
#standard error of the mean (sem)
#instantaneous
g_ABS60_inst_sem=g_ABS60_inst_sd/sqrt(g_ABS60_inst_n)
#1 hr rolling mean
g_ABS60_1hroll_sem=frollmean(g_ABS60_inst_sem,3600,align="center",na.rm=TRUE)
#95% confidence interval (ci)
#upper bound
#instantaneous
g_ABS60_inst_ci_lower<-g_ABS60_inst_avg+qt((1-0.95)/2,df=g_ABS60_inst_n-1)*g_ABS60_inst_sem
#1 hr rolling mean
g_ABS60_1hroll_ci_lower=frollmean(g_ABS60_inst_ci_lower,3600,align="center",na.rm=TRUE)
#lower bound
#instantaneous
g_ABS60_inst_ci_upper<-g_ABS60_inst_avg-qt((1-0.95)/2,df=g_ABS60_inst_n-1)*g_ABS60_inst_sem
#1 hr rolling mean
g_ABS60_1hroll_ci_upper=frollmean(g_ABS60_inst_ci_upper,3600,align="center",na.rm=TRUE)
#bind metrics to data frame
g_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,
                        g_ABS60_inst_n,g_ABS60_1hroll_n,
                        g_ABS60_inst_avg,g_ABS60_1hroll_avg,
                        g_ABS60_inst_sd,g_ABS60_1hroll_sd,
                        g_ABS60_inst_sem,g_ABS60_1hroll_sem,
                        g_ABS60_inst_ci_lower,g_ABS60_1hroll_ci_lower,
                        g_ABS60_inst_ci_upper,g_ABS60_1hroll_ci_upper)


##bind series to new data frame(s)##
#individual unit means#
unit_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,
                           #ap1d_ABS60_inst_avg=ap1d_ABS60_avg$ap1d_ABS60_inst_avg,
                           ap1d_ABS60_1hroll_avg=ap1d_ABS60_avg$ap1d_ABS60_1hroll_avg,
                           #ap2d_ABS60_inst_avg=ap2d_ABS60_avg$ap2d_ABS60_inst_avg,
                           ap2d_ABS60_1hroll_avg=ap2d_ABS60_avg$ap2d_ABS60_1hroll_avg,
                           #cr2d_ABS60_inst_avg=cr2d_ABS60_avg$cr2d_ABS60_inst_avg,
                           cr2d_ABS60_1hroll_avg=cr2d_ABS60_avg$cr2d_ABS60_1hroll_avg,
                           #cr3d_ABS60_inst_avg=cr3d_ABS60_avg$cr3d_ABS60_inst_avg,
                           cr3d_ABS60_1hroll_avg=cr3d_ABS60_avg$cr3d_ABS60_1hroll_avg,
                           
                           #ap1g_ABS60_inst_avg=ap1g_ABS60_avg$ap1g_ABS60_inst_avg,
                           ap1g_ABS60_1hroll_avg=ap1g_ABS60_avg$ap1g_ABS60_1hroll_avg,
                           #ap2g_ABS60_inst_avg=ap2g_ABS60_avg$ap2g_ABS60_inst_avg,
                           ap2g_ABS60_1hroll_avg=ap2g_ABS60_avg$ap2g_ABS60_1hroll_avg,
                           #cr2g_ABS60_inst_avg=cr2g_ABS60_avg$cr2g_ABS60_inst_avg,
                           cr2g_ABS60_1hroll_avg=cr2g_ABS60_avg$cr2g_ABS60_1hroll_avg)
                        
#treatment means#
trt_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,
                          #d_ABS60_inst_avg=d_ABS60_avg$d_ABS60_inst_avg,
                          d_ABS60_1hroll_avg=d_ABS60_avg$d_ABS60_1hroll_avg,
                          
                          #g_ABS60_inst_avg=g_ABS60_avg$g_ABS60_inst_avg,
                          g_ABS60_1hroll_avg=g_ABS60_avg$g_ABS60_1hroll_avg)

#treatment SE#
trtSE_ABS60_avg<-data.frame(time_sm_dstadj_s=tcp_ABS60$time_sm_dstadj_s,
                            d_ABS60_1hroll_avg=d_ABS60_avg$d_ABS60_1hroll_avg,
                            #d_ABS60_inst_sem=d_ABS60_avg$d_ABS60_inst_sem,
                            d_ABS60_1hroll_sem=d_ABS60_avg$d_ABS60_1hroll_sem,
                            
                            g_ABS60_1hroll_avg=g_ABS60_avg$g_ABS60_1hroll_avg,
                            #g_ABS60_inst_sem=g_ABS60_avg$g_ABS60_inst_sem,
                            g_ABS60_1hroll_sem=g_ABS60_avg$g_ABS60_1hroll_sem)

                          #d_ABS60_inst_n=d_ABS60_avg$d_ABS60_inst_n,
                          #d_ABS60_1hroll_n=d_ABS60_avg$d_ABS60_1hroll_n,
                          #d_ABS60_inst_sd=d_ABS60_avg$d_ABS60_inst_sd,
                          #d_ABS60_1hroll_sd=d_ABS60_avg$d_ABS60_1hroll_sd,
                          #d_ABS60_inst_ci_lower=d_ABS60_avg$d_ABS60_inst_ci_lower,
                          #d_ABS60_1hroll_ci_lower=d_ABS60_avg$d_ABS60_1hroll_ci_lower,
                          #d_ABS60_inst_ci_upper=d_ABS60_avg$d_ABS60_inst_ci_upper,
                          #d_ABS60_1hroll_ci_upper=d_ABS60_avg$d_ABS60_1hroll_ci_upper,

                          #g_ABS60_inst_n=g_ABS60_avg$g_ABS60_inst_n,
                          #g_ABS60_1hroll_n=g_ABS60_avg$g_ABS60_1hroll_n,
                          #g_ABS60_inst_sd=g_ABS60_avg$g_ABS60_inst_sd,
                          #g_ABS60_1hroll_sd=g_ABS60_avg$g_ABS60_1hroll_sd,
                          #g_ABS60_inst_ci_lower=g_ABS60_avg$g_ABS60_inst_ci_lower,
                          #g_ABS60_1hroll_ci_lower=g_ABS60_avg$g_ABS60_1hroll_ci_lower,
                          #g_ABS60_inst_ci_upper=g_ABS60_avg$g_ABS60_inst_ci_upper,
                          #g_ABS60_1hroll_ci_upper=g_ABS60_avg$g_ABS60_1hroll_ci_upper)

#convert data frame(s) to tidy structure#
unit_ABS60_avg_tidy<-gather(unit_ABS60_avg,var,val,-time_sm_dstadj_s)
trt_ABS60_avg_tidy<-gather(trt_ABS60_avg,var,val,-time_sm_dstadj_s)
#trtSE_ABS60_avg_tidy<-gather(trtSE_ABS60_avg,var,val,-time_sm_dstadj_s)
#preview tidy data
head(unit_ABS60_avg_tidy)
head(trt_ABS60_avg_tidy)

##plot 1 hr rolling mean time series for each treatment##
plot.dose.burn<-ggplot() +
  
  #treatment means#
  geom_line(trt_ABS60_avg_tidy,
            mapping=aes(x=time_sm_dstadj_s,y=val,color=var,alpha=var),
            size=1.5) +
  
  #define colors
  scale_color_manual(name=element_blank(),
                     breaks=c("d_ABS60_1hroll_avg",
                              "g_ABS60_1hroll_avg"),
                     labels=c("DS (all), with std err",
                              "GS (all), with std err"),
                     values=c("sienna4","green4")) + #D and G
  
  #define alpha values (transparency)
  scale_alpha_manual(name=element_blank(),
                     breaks=c("d_ABS60_1hroll_avg",
                              "g_ABS60_1hroll_avg"),
                     labels=c("DS (all), with std err",
                              "GS (all), with std err"),
                     values=c(1,1)) + #D and G
  
  #treatment SE#
  #dormant season
  geom_ribbon(trtSE_ABS60_avg,
              mapping=aes(x=time_sm_dstadj_s,
                          ymin=d_ABS60_1hroll_avg-(d_ABS60_1hroll_sem/2),
                          ymax=d_ABS60_1hroll_avg+(d_ABS60_1hroll_sem/2)),
              fill="sienna4",
              alpha=0.35) +
  #growing season
  geom_ribbon(trtSE_ABS60_avg,
              mapping=aes(x=time_sm_dstadj_s,
                          ymin=g_ABS60_1hroll_avg-(g_ABS60_1hroll_sem/2),
                          ymax=g_ABS60_1hroll_avg+(g_ABS60_1hroll_sem/2)),
              fill="green4",
              alpha=0.35) +

  labs(x="Time of burn day (hh:mm)",
       y="1 hr moving average of thermocouple\nheating for temperatures \u2265 60 °C (°C s)") +
  theme_classic() +
  theme(axis.text.x=element_text(size=11),
        axis.title.x=element_text(face="bold",color="black",size=15),
        axis.text.y=element_text(size=11),
        axis.title.y=element_text(face="bold",color="black",size=15),
        legend.position=c(0.8,0.9),
        legend.background=element_rect(fill="gray90"),
        legend.title=element_blank(),
        legend.text=element_text(color="black",size=12),
        legend.key.width=unit(2,"line")) +
  scale_x_continuous(limits=c(41400,66600),
                     breaks=c(43200,50400,57600,64800),
                     labels=c("12:00","14:00","16:00","18:00"),
                     expand=c(0,0))
plot.dose.burn
#save to file
png("Figures/MS 1/fig7rev_mean_dose_time_series.png",width=6.5,height=5,units="in",res=600)
plot.dose.burn
dev.off()