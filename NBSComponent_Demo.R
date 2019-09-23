##A script to put annual average and potentially decadal averages of each variable on the same plot as the monthly averages
##Using code from annualavgs.R and pre_ev_ro_monthly.R

##Created August 2018 by DCA contact: deanna.apps@usace.army.mil

##Setup the work space
proj.dir <- '//155.79.129.200/lre1-public/Public/SHARE/H&H/GL_H&HData & Reports/WaterBudgetTracking/TrendGraphics'
setwd(proj.dir)

wb.dir <- '//155.79.129.200/lre1-public/Public/SHARE/H&H/GL_H&HData & Reports/WaterBudgetTracking'
precip.dir<- 'precipitation/data'
runoff.dir<-'runoff/data'
evap.dir<-'evaporation/data'
ice.dir<-'ice/data'
water.dir<-'SurfaceWaterTemps/data'
wl.dir <- 'WaterLevels/data/LWA_Monthly'
logo.dir<-'/Plotting/'


library(data.table)
library(ggplot2)
library(zoo)
library(hydroTSM)
library(stringr)
library(gridExtra)
library(grid)
library(ggthemes)
library(png)

##Read Initiation file to determine end year for plots
file<-readLines(paste(proj.dir, 'INIT_NBS.txt', sep="/"))
startyear<-strsplit(file, split=" ") [[1]] [3]
endyear<-strsplit(file, split=" ") [[2]] [3]
number<-strsplit(file, split=" ") [[3]] [2]
number<-as.numeric(number)

folder_name<-paste(startyear, endyear, sep="_")

# create a folder in which to store the plots *There are two different directories depending on if you're running for the annula update or just a specific time period

#if (number == 1){
#  plot.dir<-paste(proj.dir, 'NBS_Plots', 'AnnualUpdate', folder_name, sep="/")
#  dir.create(plot.dir, recursive=TRUE, showWarnings = FALSE)
#}else if (number == 2) {
#    plot.dir<-paste(proj.dir, 'NBS_Plots', 'SpecificTimePeriods', folder_name, sep="/")
#    dir.create(plot.dir, recursive=TRUE, showWarnings = FALSE)
#  }


## Load the data
##Precipitation 
precip.dat<-read.csv(paste(wb.dir, precip.dir, 'precip.monthly.csv', sep='/'), skip=7)

##Allow us to be able to look at the period of 1950 through the year just ending
precip.data<-precip.dat[precip.dat$year>=startyear & precip.dat$year<=endyear,]

precip.data2<-zoo(precip.data[,3:7], order.by=as.Date(paste(precip.data$year, precip.data$month, 15, sep='-')))
precip.sum<-monthly2annual(precip.data2, FUN=sum)

##Runoff (AHPS)
runoff.data<-read.csv(paste(wb.dir, runoff.dir, 'runoff.monthly.AHPS.csv', sep='/'), skip=5)
runoff.data<-runoff.data[runoff.data$year>=startyear & runoff.data$year<=endyear,]
runoff.data<-runoff.data[,c(1,2,3,7,8,9,10)]
runoff.data2<-zoo(runoff.data[,3:7], order.by=as.Date(paste(runoff.data$year, runoff.data$month, 15, sep='-')))
runoff.sum<-monthly2annual(runoff.data2, FUN=sum)

##Evap (AHPS)
evap.data<-read.csv(paste(wb.dir, evap.dir, 'evaporation.monthly.AHPS.csv', sep='/'), skip=5)
evap.data<-evap.data[evap.data$year>=startyear & evap.data$year<=endyear,]
evap.data<-evap.data[,c(1,2,3,7,8,9,10)]
evap.data2<-zoo(evap.data[,3:7], order.by=as.Date(paste(evap.data$year, evap.data$month, 15, sep='-')))
evap.sum<-monthly2annual(evap.data2, FUN=sum)


lakes<-c("SUP", "MIH", "STC", "ERI", "ONT")
lakes.full<-c("Superior", "Michigan-Huron", "St. Clair", "Erie", "Ontario")

img<-readPNG(paste(wb.dir,logo.dir,"CorpsLogo.png", sep=""))

##MONTH LOOP for P, E and R
for (i in 1:length(lakes)) {
  lk<-lakes[i]
  lks<-lakes.full[i]
  ##FOR MONTHLY PRECIPITATION
  precip<-precip.data[, c(1,2,i+2)]
  precip$month<-month.abb[precip$month]
  precip$month<-factor(precip$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
 
   ###GGPLOT by month
  p<-ggplot(data=precip, 
            aes(x=year, y=get(paste(lk)), group=month)) +
    geom_bar(stat="identity", aes(fill=month)) +
    ggtitle(paste('Lake', lks, 'Monthly Precipitation', sep=' ')) +
    ylab(label="Precipitation (mm)") +
    xlab(label="Year")+
    guides(fill=FALSE) +
    scale_x_continuous(breaks=seq(1940, 2010, 10)) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Precipitation (in)", breaks=derive())) +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    facet_wrap(~month, ncol=12)
  plot<-p + theme_classic()
  plot1<-plot + theme(text = element_text(size=14, face="bold"),
                  axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=14), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=14, hjust=0.5, face="bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), panel.background=element_rect(fill="gray", colour="black", size=1, linetype="solid"))
  
  ##FOR MONTHLY EVAPORATION
  evap<-evap.data[, c(1,2,i+2)]
  evap$month<-month.abb[evap$month]
  evap$month<-factor(evap$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  ###GGPLOT by month
  p<-ggplot(data=evap, 
            aes(x=year, y=get(paste(lk)), group=month)) +
    geom_bar(stat="identity", aes(fill=month)) +
    ggtitle(paste('Lake', lks, 'Monthly Evaporation', sep=' ')) +
    ylab(label="Evaporation (mm)") +
    xlab(label="Year")+
    scale_x_continuous(breaks=seq(1940, 2010, 10)) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Evaporation (in)", breaks=derive())) +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    guides(fill=FALSE) +
    facet_wrap(~month, ncol=12)
  plot2<-p + theme_classic()
  plot3<-plot2 + theme(text = element_text(size=14, face="bold"),
                  axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=14), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=14, hjust=0.5, face="bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), panel.background=element_rect(fill="gray", colour="black", size=1, linetype="solid"))
  
  
  ##FOR MONTHLY RUNOFF
  runf<-runoff.data[, c(1,2,i+2)]
  runf$month<-month.abb[runf$month]
  runf$month<-factor(runf$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  ###GGPLOT by month
  p<-ggplot(data=runf, 
            aes(x=year, y=get(paste(lk)), group=month)) +
    geom_bar(stat="identity",  aes(fill=month)) +
    ggtitle(paste('Lake', lks, 'Monthly Runoff', sep=' ')) +
    ylab(label="Runoff (mm)") +
    xlab(label="Year")+
    scale_x_continuous(breaks=seq(1940, 2010, 10)) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Runoff (in)", breaks=derive())) +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    guides(fill=FALSE) +
    facet_wrap(~month, ncol=12)
  plot4<-p + theme_classic()
  plot5<-plot4 + theme(text = element_text(size=14, face="bold"),
                  axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=14), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=14, hjust=0.5, face="bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), panel.background=element_rect(fill="gray", colour="black", size=1, linetype="solid"))
  
  ##Add logo
  my_g <- grobTree(rectGrob(gp=gpar(fill="white", lty="blank")), rasterGrob(img, x=0.02, y= 0.9, hjust=0.1))
  
  
  mon_nbs<-grid.arrange(arrangeGrob(plot1, plot3, plot5, ncol=1), my_g, heights=c(9,0.75), top=textGrob(paste('Lake', lks, 'Monthly NBS Components'), gp=gpar(fontsize=20, font =4)))
  ggsave(mon_nbs, filename=paste(plot.dir, '/', lk, "_mon_nbs.pdf", sep=""), width=40, height=21, units=c("cm"))
  ggsave(mon_nbs, filename=paste(plot.dir, '/', lk, "_mon_nbs.png", sep=""), width=40, height=21, units=c("cm"))
  
}

##END MONTH LOOP

##BEGIN YEAR LOOP

for (i in 1:length(lakes)) {
  lk<-lakes[i]
  lks<-lakes.full[i]

  ##FOR ANNUAL Precipitation
  sumpr<-precip.sum[, i]
  precip<-as.data.frame(sumpr)
  colnames(precip)<-c("value")
  precip$year<-as.Date(rownames(precip))
  precip$year<-format(precip$year, format="%Y")
  precip$variable<-c("Precipitation")
  
  ##GGPLOT by YEAR
  p1<-ggplot(data=precip, 
             aes(x=year, y=value, group=variable)) +
    geom_bar(stat="identity", colour=c("blue")) +
    ggtitle(paste('Lake', lks, 'Annual Precipitation', sep=' ')) +
    xlab(label="Year")+
    ylab(label="Precipitation (mm)") +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Precipitation (in)", breaks=derive())) +
    scale_x_discrete(breaks=seq(1940, 2015, 5)) 
  plot<-p1 + theme_classic()
  plot1<-plot + theme(text = element_text(size=12, face="bold"),
                    axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=12, hjust=0.5, face="bold"), plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"), panel.background=element_rect(fill="gray", colour="black"))
  
  
  ##FOR ANNUAL EVAPORATION
  sumev<-evap.sum[, i]
  evap<-as.data.frame(sumev)
  colnames(evap)<-c("value")
  evap$year<-as.Date(rownames(evap))
  evap$year<-format(evap$year, format="%Y")
  evap$variable<-c("Evaporation")
  

  
  ##GGPLOT by YEAR
  p2<-ggplot(data=evap, 
             aes(x=year, y=value, group=variable)) +
    geom_bar(stat="identity", colour=c("orange")) +
    ggtitle(paste('Lake', lks, 'Annual Evaporation', sep=' ')) +
    xlab(label="Year")+
    ylab(label="Evaporation (mm)") +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Evaporation (in)", breaks=derive())) +
    scale_x_discrete(breaks=seq(1940, 2015,5)) 
  plot2<-p2 + theme_classic()
  plot3<-plot2 + theme(text = element_text(size=12, face="bold"),
                    axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=12, hjust=0.5, face="bold"), plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"), panel.background=element_rect(fill="gray", colour="black"))
  
  ##FOR ANNUAL Runoff
  sumrf<-runoff.sum[, i]
  runf<-as.data.frame(sumrf)
  colnames(runf)<-c("value")
  runf$year<-as.Date(rownames(runf))
  runf$year<-format(runf$year, format="%Y")
  runf$variable<-c("Runoff")
  
  
  ##GGPLOT by YEAR
  p3<-ggplot(data=runf, 
             aes(x=year, y=value, group=variable)) +
    geom_bar(stat="identity", colour=c("green")) +
    ggtitle(paste('Lake', lks, 'Annual Runoff', sep=' ')) +
    xlab(label="Year")+
    ylab(label="Runoff (mm)") +
    geom_smooth(method="loess", colour="black", se=FALSE) +
    scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Runoff (in)", breaks=derive())) +
    scale_x_discrete(breaks=seq(1940, 2015, 5))
  plot4<-p3 + theme_classic()
  plot5<-plot4 + theme(text = element_text(size=12, face="bold"),
                    axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=12, hjust=0.5, face="bold"), plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"), panel.background=element_rect(fill="gray", colour="black"))
  
  ##Add logo
  my_g <- grobTree(rectGrob(gp=gpar(fill="white", lty="blank")), rasterGrob(img, x=0.02, y= 0.8, hjust=0.1))
  
  
  ann_nbs<-grid.arrange(arrangeGrob(plot1, plot3, plot5, ncol=1), my_g, heights=c(7,0.35), top=textGrob(paste('Lake', lks, 'Annual NBS Components'), gp=gpar(fontsize=16, font =4)))
  ggsave(ann_nbs, filename=paste(plot.dir, '/', lk, "_ann_nbs.pdf", sep=""), width=14, height=23, units=c("cm"))
  ggsave(ann_nbs, filename=paste(plot.dir, '/', lk, "_ann_nbs.png", sep=""), width=14, height=23, units=c("cm"))
}

##END OF YEAR LOOP

##***************BELOW WOULD COINCIDE WITH USING FACET WRAP FOR THE ANNUAL PLOT FOR ONE LAKE*************
#all.data<-rbind(precip,evap,runf)

#all.data$variable<-factor(all.data$variable, levels=c("Precipitation", "Evaporation", "Runoff"))  

#p5<-ggplot(data=all.data, 
#           aes(x=year, y=value, group=variable)) +
#  geom_bar(stat="identity", aes(colour=variable)) +
#  ggtitle(paste('Lake', lks, 'Annual NBS Components', sep=' ')) +
#  xlab(label="Year")+
#  ylab(label="Millimeters") +
#  geom_smooth(method="loess", colour="black", se=FALSE) +
#  scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Inches", breaks=derive())) +
#  scale_x_discrete(breaks=seq(1940, 2015, 5))+
#  scale_colour_manual(values=c("Precipitation" = "blue", "Evaporation" ="orange", "Runoff" ="green"))+
#  guides(colour=FALSE) +
#  facet_rep_wrap(~variable, ncol=1, repeat.tick.labels = TRUE, scales="free_y", labeller = labeller(variable = c("Precipitation" = paste("Lake", lks, "Annual Precipitation", sep=" "), "Evaporation" = paste("Lake", lks, "Annual Evaporation", sep=" "), "Runoff" = paste("Lake", lks, "Annual Runoff", sep=" "))))


#plot4<-p5 + theme_classic()
#plot5<-plot4 + theme(text = element_text(size=12, face="bold"),
#                     axis.text.x = element_text(color="black", size=12, angle=90, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=12, hjust=0.5, face="bold"), plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"), panel.background=element_rect(fill="gray", colour="black"))

#plot.dir <- '//155.79.129.200/lre1-public/Public/SHARE/H&H/WHS/Deanna'

##Add logo
#my_g <- grobTree(rectGrob(gp=gpar(fill="white", lty="blank")), rasterGrob(img, x=0.02, y= 0.8, hjust=0.1))


#ann_nbs<-grid.arrange(arrangeGrob(plot5, ncol=1), my_g, heights=c(7,0.35))
#ggsave(ann_nbs, filename=paste(plot.dir, "ann_demo.png", sep="/"), width=14, height=23, units=c("cm"))



##*****************************TO CREATE THE GGPLOT AS AN INTERACTIVE HTML FOLLOW STEPS BELOW****************************
###PLOTLY PLOT
#library(plotly)
#library(htmlwidgets)
#library(devtools)

#p6<-ggplot(data=all.data, 
#           aes(x=year, y=value, group=variable, text=paste('Variable: ', variable, '\n', 'Year: ', year, '\n', 'Total (mm): ', value, '\n'))) +
#  geom_bar(stat="identity", aes(colour=variable)) +
#  ggtitle(paste('Lake', lks, 'Annual NBS Components', sep=' ')) +
 # xlab(label="Year")+
 # ylab(label="Millimeters") +
 # geom_smooth(method="loess", colour="black", se=FALSE) +
 # scale_y_continuous(sec.axis= sec_axis(~.*0.03937008, name= "Inches", breaks=derive())) +
 # scale_x_discrete(breaks=seq(1940, 2015, 5))+
 # scale_colour_manual(values=c("Precipitation" = "blue", "Evaporation" ="orange", "Runoff" ="green"))+
 # guides(colour=FALSE) +
#  facet_rep_wrap(~variable, ncol=1, repeat.tick.labels = TRUE, scales="free_y", labeller = labeller(variable = c("Precipitation" = paste("Lake", lks, "Annual Precipitation", sep=" "), "Evaporation" = paste("Lake", lks, "Annual Evaporation", sep=" "), "Runoff" = paste("Lake", lks, "Annual Runoff", sep=" "))))


#plot6<-p6 + theme_classic()
#plot7<-plot6 + theme(text = element_text(size=12, face="bold"),
#                     axis.text.x = element_text(color="black", size=12, hjust=0.5, vjust=0.5), axis.text.y = element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold"), axis.title.y=element_text(face="bold"), plot.title=element_text(size=12, hjust=0.5, face="bold"), plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"), panel.background=element_rect(fill="gray", colour="black"))


#widgetnbsm <- ggplotly(plot7, tooltip="text", width=1500, height=1200)
#saveWidget(widgetnbsm, paste(plot.dir, "ann_demo2.html", sep="/"), selfcontained = TRUE)

  
