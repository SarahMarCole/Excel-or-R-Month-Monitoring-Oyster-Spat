#Plotting Monthly Spat Averages in the IRL and SLE

#Setting up working directory and saving working environment
setwd("~/SPAT")
load(".RData")
save.image()

#Installing & Loading Packages 
  #*only need to install once***#--- this will install if you don't have them
  #*but won't if you already do

if(!require(dplyr)){install.packages("dplyr")}
if(!require(imager)){install.packages("imager")}
if(!require(lubridate)){install.packages("lubridate")}

#*****Load Packages****#
  
library(dplyr)
library(imager)
library(lubridate)

#load data into working environment
  #read data into a variable ***UPDATE EACH MONTH***#
#spatdata<-read.csv("master_SpatCounts.csv",header=TRUE)
library(readxl)
spatdata <- read_excel("/master_SpatCounts.xlsx")


#checking and changing column formatting
 
  #overview of column format
#str(spatdata)
  
    #date field to date
spatdata$date <-as.Date(spatdata$date, format="%Y-%m-%d")
    

#Adding a month column to the dataframe
spatdata$month <- month(spatdata$date)
#spatdata$month<-sort(spatdata$month)
spatdata$month<-month.abb[spatdata$month]
#Adding a year column to the dataframe
spatdata$year <- year(spatdata$date)

#month_year column for stats
spatdata$month_year<-paste(spatdata$month,"_", spatdata$year)

#Create a table of spat averages and standard deviation by month -------
se <- function(x) sqrt(var(x) / length(x))
monthyearavgspat <- aggregate(spat~month+year+location+date, 
                              data=spatdata, FUN=mean)
monthyearavgspat$sd<-aggregate(spat ~month+year+location+date, 
                               data=spatdata, FUN=se)
#turn months into character abbreviations
monthyearavgspat$month<-month.abb[monthyearavgspat$month]

monthyearavgspat$month_year<-paste(monthyearavgspat$month, monthyearavgspat$year)


#Month Spat Figure---------------------
#separate in IRL and SLE
spat_IRL<-dplyr::filter(monthyearavgspat, location=="IRL")
spat_SLE<-filter(monthyearavgspat, location=="SLE")


plot.new()
par(mar = c(5, 5, 2, 2))#set up size of plot
plot(spat_IRL$date, spat_IRL$spat, ylim=c(0,75), ylab="",  #ylim might need to be changed based on max(mean+sd)
     xlab = "", pch=21, bg="chartreuse3", cex=2, cex.lab=2, xaxt="n", yaxt="n")
axis(side=2, at=c(0,10,20,30,40,50,60,70), cex.axis=2)
axis(1, at = spat_IRL$date, labels =FALSE) 
#set y axis
title(ylab="Average Number of Spat per Shell", line=3, cex.lab=2)
#set trendlines

lines(spat_IRL$date,spat_IRL$spat,col="chartreuse3",lwd=3)
lines(spat_SLE$date, spat_SLE$spat, col="deepskyblue1", lwd=3)

#standard error bars
arrows(spat_IRL$date, spat_IRL$spat-spat_IRL$sd$spat ,spat_IRL$date, spat_IRL$spat+spat_IRL$sd$spat, 
       length=0.05, angle=90, code=3, col="chartreuse3")
arrows(spat_SLE$date, spat_SLE$spat-spat_SLE$sd$spat ,spat_SLE$date, spat_SLE$spat+spat_SLE$sd$spat, 
       length=0.05, angle=90, code=3, col="deepskyblue1")
#put points on after so that easier to see above lines-- IRL is plotted again to go over lines 
points(spat_SLE$date, spat_SLE$spat, pch=22, bg="deepskyblue1", cex=2)
points(spat_IRL$date, spat_IRL$spat, pch=21, bg="chartreuse3", cex=2)

#add line for 2021
abline(v=as.Date("2021-01-01"), lty=2)
abline(v=as.Date("2022-01-01"), lty=2)
abline(v=as.Date("2023-01-01"), lty=2)
abline(h=0)
#add dates to x axis

text(x=(spat_IRL$date-1), par("usr")[3]-4, labels=spat_IRL$sd$month, srt=90, pos=1, xpd=TRUE, cex=1.5)

#add 2021 label
text(x=as.Date("2020-12-15"), y=40, labels="2021", cex=1.5, srt=45)
text(x=as.Date("2021-12-12"), y=40, labels="2022", cex=1.5, srt=45)
text(x=as.Date("2022-12-12"), y=40, labels="2023", cex=1.5, srt=45)

#add legend
legend(#x=min(spat_SLE$date)-10, y=(max(spat_SLE$spat+spat_SLE$sd$spat)-25),
        "top",cex=1.5,
        c("Indian River Lagoon","St.Lucie Estuary"), 
        bty = "n",pt.cex=3,
        pch=c(21,22),
        pt.bg=c("chartreuse3", "deepskyblue1"))

#*export image with width 1650, height 600**
#*
#*
