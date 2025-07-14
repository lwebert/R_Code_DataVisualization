
#################################Violin Plots##################################

library(readxl)
library(ggplot2)
library(uniReg)
library(ggimage)

## Install packages
library("plyr")
library("lattice")
library("ggplot2")
library("dplyr")
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")
library(ggimage)
library(readxl)
library(tidyr)
library(stringr)


### Can adjust the following:

#Change the color of the violin plots - just paste the HEX code over the numbers here 
Stim1_Young = "#f5a478"
Stim1_Old = "#78f5ae"
Stim2_Young = "#f5a478"
Stim2_Old = "#78f5ae"

#WIDTH (left/right) dodge value in geom_point. 
#Dodge adjusts the horizontal position of an object (aka adjusts the "center" of geom_point that the points are then jittered around)...
pnt_offset_from_center = .5

#WIDTH (left/right) jitter in geom_point.
#Jitter adds random noise, so will move the points to avoid overlapping data from the "center"/dodge of geom_point.
jitter = .04





### Load the data
setwd('D:\\z_Other_not project specific\\R Coding Practice\\Help Yasra')

data <- read_xlsx('gamma_R.xlsx', sheet = 1, col_names = TRUE)

#Select a subset of the data by the column # in the excel. EXAMPLE c(1,3,5) will select columns 1,3, and 5.
data1 <- data[c(1:4)]

#Create a new "column" that groups together the 2 variables you want plotted on the x-axis
data1$StimAge <- paste(data$stim,"_",data$age,sep = '')

data1$age <- as.factor(data$age)
data1$stim <- as.factor(data$stim)





###PLOT
#Note - can move where in the script geom_violin, geom_boxplot, and geom_point are depending on how you want it layered
#Note - some of the colors here are typed as 'black', but you can replace with hex codes '#000000'

p <- ggplot(data = na.omit(data1), aes(x=StimAge, y=gamma_cathode1, fill=StimAge)) + 
  #violin - change the alpha (transparency), trim (TRUE or FALSE; whether it ends flat or at a point), position_dodge (moves the "center" of the violin by whatever # you input)
  geom_violin(alpha = 0.8, aes(fill=StimAge), trim=FALSE, color='black', position=position_dodge(1)) +
  
  #boxplot - change width (of the box), fatten (thickness of median line), position_dodge (moves the "center" of the violin by whatever # you input)
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  #points - change position (jitter & dodge), color *OUT of aes()* to set the outline color, fill *OUT of aes()* to set the dot fill color, alpha *INSIDE aes()* to change the transparency of the dots, size *OUT of aes()* to set the dot size
  geom_point(position = position_jitterdodge(jitter.width=jitter, dodge.width=pnt_offset_from_center), aes(color=StimAge, fill=StimAge, alpha=0.3), color = 'black', fill = 'black', size = 1, shape = 21) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Stim1_Young, Stim1_Old, Stim2_Young, Stim2_Old)) +
  scale_fill_manual(values=c(Stim1_Young, Stim1_Old, Stim2_Young, Stim2_Old)) +
  
  #code to make the X that marks the mean on each plot
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  
  #code to draw the lines between violin plots - change aes(group=XXX) to determine what plots the lines should go between
  stat_summary(fun.y=mean, geom="line", aes(group=age)) +
  
  #coord_cartesian(ylim = c(-3,12), xlim(c(0,50))) +   #OPTIONAL - sets specific limits to the y-axis and/or x-axis
  theme(aspect.ratio = 1)   #Makes the plot a square when you plot & save it

plot(p)


ggsave("Plot_Name", width = 6, height = 6)


