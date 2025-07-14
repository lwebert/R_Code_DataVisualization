
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

axis_text_text_size = 15
axis_title_text_size = 20

best_fit_line_color = "#000000"
best_fit_SEM_color  = "#DBDBDB"

dot_Color       = "#000000"
dot_size            = 2.5

# width and height variables for saved plots
w = 6
h = 4
# Define limits of y-axis
y_lim_min = 4
y_lim_max = 7.5

#fonts
loadfonts(device="win")
font_import()
fonts()

Color1 = "#20A4F3"
Color2 = "#68D89B"
Color3 = "#D56062"




setwd('D:\\SCAN_OneDotGamma\\Derivatives\\R')

data <- read_xlsx('R_Plot_Gamma_by_entrainment_condition.xlsx', sheet = 1, col_names = TRUE)


##############################Original Data##########################################

########### Oscillatory Average Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillAvg <- data[c(1:2,4)]

p <- ggplot(data = na.omit(data_RH_OscillAvg), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillAvg_noOutliers, fill=Freq_Condition_RH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +

  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Average_Gamma_RHPeaks_byFrequency.tiff", width = 7, height = 5)





########### Oscillatory Max Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillMax <- data[c(1:2,5)]

p <- ggplot(data = na.omit(data_RH_OscillMax), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillMax_noOutliers, fill=Freq_Condition_RH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Max_Gamma_RHPeaks_byFrequency.tiff", width = 7, height = 5)




########### Oscillatory Average Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillAvg <- data[c(1,3,6)]

p <- ggplot(data = na.omit(data_LH_OscillAvg), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillAvg_noOutliers, fill=Freq_Condition_LH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Average_Gamma_LHPeaks_byFrequency.tiff", width = 7, height = 5)





########### Oscillatory Max Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillMax <- data[c(1,3,7)]

p <- ggplot(data = na.omit(data_LH_OscillMax), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillMax_noOutliers, fill=Freq_Condition_LH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Max_Gamma_LHPeaks_byFrequency.tiff", width = 7, height = 5)









##############################Cube Root Transformation##########################################

########### Oscillatory Average Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillAvg_cuberoot <- data[c(1:2,8)]

p <- ggplot(data = na.omit(data_RH_OscillAvg_cuberoot), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillAvg_CubeRoot_noOutliers, fill=Freq_Condition_RH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Average_Gamma_RHPeaks_byFrequency_cuberoot.tiff", width = 7, height = 5)





########### Oscillatory Max Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillMax_cuberoot <- data[c(1:2,9)]

p <- ggplot(data = na.omit(data_RH_OscillMax_cuberoot), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillMax_CubeRoot_noOutliers, fill=Freq_Condition_RH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Max_Gamma_RHPeaks_byFrequency_cuberoot.tiff", width = 7, height = 5)




########### Oscillatory Average Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillAvg_cuberoot <- data[c(1,3,10)]

p <- ggplot(data = na.omit(data_LH_OscillAvg_cuberoot), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillAvg_CubeRoot_noOutliers, fill=Freq_Condition_LH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Average_Gamma_LHPeaks_byFrequency_cuberoot.tiff", width = 7, height = 5)





########### Oscillatory Max Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillMax_cuberoot <- data[c(1,3,11)]

p <- ggplot(data = na.omit(data_LH_OscillMax_cuberoot), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillMax_CubeRoot_noOutliers, fill=Freq_Condition_LH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Max_Gamma_LHPeaks_byFrequency_cuberoot.tiff", width = 7, height = 5)








########### Oscillatory Avg Gamma (averaged across hemi) by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")


p <- ggplot(data, aes(x=FreqCond_AcrossHemi, y=Gamma_OscillAvg_AcrossHemi_NoOutliers, fill=FreqCond_AcrossHemi)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=FreqCond_AcrossHemi,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Avg_Gamma_AcrossHemi_byFrequency.tiff", width = 7, height = 5)








########### Oscillatory Max Gamma (averaged across hemi) by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillMax <- data[c(1:2,5)]

p <- ggplot(data = na.omit(data_RH_OscillMax), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillMax_noOutliers, fill=Freq_Condition_RH)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Color1, Color2, Color3)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  ggtitle("") +
  theme(plot.title = element_blank()) + 
  #theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Frequency Condition") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  #theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) +
  theme(axis.title.y = element_blank()) +
  #theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(0,0.4))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")


#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("ViolinPlot_Oscill_Max_Gamma_AcrossHemi_byFrequency.tiff", width = 7, height = 5)





