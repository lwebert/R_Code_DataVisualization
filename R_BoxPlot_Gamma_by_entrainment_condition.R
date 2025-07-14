
#################################Box Plots##################################

library(readxl)
library(ggplot2)
library(uniReg)
library(ggimage)

## Install packages
library("plyr")
library("lattice")
library("ggplot2")
library(dplyr)
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")
library(ggimage)
library(readxl)
library(tidyr)
library(stringr)

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

#mean_y <- data[c(1:2,12:15)]

mean_line <- data %>% group_by(Freq_Condition_RH) %>% summarize(average = mean(Gamma_RH_OscillAvg_mean)) %>% ungroup()


p <- ggplot(data = na.omit(data_RH_OscillAvg), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillAvg_noOutliers, fill=Freq_Condition_RH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_RH)) + 
  geom_boxplot(width = .5, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_RH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=5, color="black", fill="black") +
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_RH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(0,7)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_OscillAverage_byEntrainCond_BoxPlot.tiff", width = 5, height = 5)





########### Oscillatory Maximum Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillMax <- data[c(1:2,5)]

mean_line <- data %>% group_by(Freq_Condition_RH) %>% summarize(average = mean(Gamma_RH_OscillMax_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_RH_OscillMax), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillMax_noOutliers, fill=Freq_Condition_RH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_RH)) + 
  geom_boxplot(width = .5, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_RH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=5, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_RH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_RH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(0,7)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_OscillMaximum_byEntrainCond_BoxPlot.tiff", width = 5, height = 5)






########### Oscillatory Average Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillAvg <- data[c(1,3,6)]

mean_line <- data %>% group_by(Freq_Condition_LH) %>% summarize(average = mean(Gamma_LH_OscillAvg_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_LH_OscillAvg), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillAvg_noOutliers, fill=Freq_Condition_LH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .5, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_LH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=5, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(0,7)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_OscillAverage_byEntrainCond_BoxPlot.tiff", width = 5, height = 5)





########### Oscillatory Maxmimum Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .6

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillMax <- data[c(1,3,7)]

mean_line <- data %>% group_by(Freq_Condition_LH) %>% summarize(average = mean(Gamma_LH_OscillMax_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_LH_OscillMax), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillMax_noOutliers, fill=Freq_Condition_LH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .5, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_LH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=5, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(0,7)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_OscillMaximum_byEntrainCond_BoxPlot.tiff", width = 5, height = 5)









#################################Box Plots with Violin Plots##################################


########### Oscillatory Average Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillAvg <- data[c(1:2,4)]

mean_line <- data %>% group_by(Freq_Condition_RH) %>% summarize(average = mean(Gamma_RH_OscillAvg_mean)) %>% ungroup()


p <- ggplot(data = na.omit(data_RH_OscillAvg), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillAvg_noOutliers, fill=Freq_Condition_RH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_RH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_RH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_RH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_OscillAverage_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)




########### Oscillatory Maximum Gamma RH by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_RH_OscillMax <- data[c(1:2,5)]

mean_line <- data %>% group_by(Freq_Condition_RH) %>% summarize(average = mean(Gamma_RH_OscillMax_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_RH_OscillMax), aes(x=Freq_Condition_RH, y=Gamma_RHLingual_OscillMax_noOutliers, fill=Freq_Condition_RH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_RH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_RH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_RH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_RH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_RH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_RH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_OscillMaximum_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)






########### Oscillatory Average Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillAvg <- data[c(1,3,6)]

mean_line <- data %>% group_by(Freq_Condition_LH) %>% summarize(average = mean(Gamma_LH_OscillAvg_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_LH_OscillAvg), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillAvg_noOutliers, fill=Freq_Condition_LH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_LH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_OscillAverage_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)





########### Oscillatory Maxmimum Gamma LH by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_LH_OscillMax <- data[c(1,3,7)]

mean_line <- data %>% group_by(Freq_Condition_LH) %>% summarize(average = mean(Gamma_LH_OscillMax_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_LH_OscillMax), aes(x=Freq_Condition_LH, y=Gamma_LHCuneus_OscillMax_noOutliers, fill=Freq_Condition_LH)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Freq_Condition_LH, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Freq_Condition_LH), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous +
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_OscillMaximum_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)













########### Oscillatory Average Gamma (averaged across hemi) by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_OscillAvg <- data[c(1,16:17)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_AcrossHemi_OscillAvg_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_OscillAvg), aes(x=FreqCond_AcrossHemi, y=Gamma_OscillAvg_AcrossHemi_NoOutliers, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous 
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_AcrossHemi_OscillAverage_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)







########### Oscillatory Maximum Gamma (averaged across hemi) by entrainment condition

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_OscillMax <- data[c(1,16,18)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_AcrossHemi_OscillMax_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_OscillMax), aes(x=FreqCond_AcrossHemi, y=Gamma_OscillMax_AcrossHemi_NoOutliers, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous 
  coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_AcrossHemi_OscillMaximum_byEntrainCond_BoxPlot_violin.tiff", width = 5, height = 5)







setwd('D:\\SCAN_OneDotGamma\\Derivatives\\R')

data <- read_xlsx('R_Plot_Gamma_by_entrainment_condition.xlsx', sheet = 3, col_names = TRUE)


##########################DICS CONNECTIVITY BOX PLOT WITH VIOLIN PLOTS########################



########### Gamma Connectivity DICS (averaged across hemi) by entrainment condition - LH Precentral peak (WB LME)

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_DICS_LH_Precentral <- data[c(1:2,5:6)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_DICS_Hemi_LME_LH_Precentral_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_DICS_LH_Precentral), aes(x=FreqCond_AcrossHemi, y=Gamma_DICS_Hemi_LME_LH_Precentral, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Connectivity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial"))+
  #scale_y_continuous 
  coord_cartesian(ylim = c(-22,44)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_DICS_IndAvg_AcrossHemi_WB-LME_LH_Precentral_byEntrainCond_BoxPlot_violin_-22to44axis.tiff", width = 5, height = 5)







########### Gamma Connectivity DICS (averaged across hemi) by entrainment condition - LH Inferior Parietal peak (WB LME)

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_DICS_LH_InfParietal <- data[c(1:2,7:8)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_DICS_Hemi_LH_InfParietal_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_DICS_LH_InfParietal), aes(x=FreqCond_AcrossHemi, y=Gamma_DICS_Hemi_LH_InfParietal, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Connectivity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous 
  coord_cartesian(ylim = c(-22,44)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_DICS_IndAvg_AcrossHemi_WB-LME_LH_InfParietal_byEntrainCond_BoxPlot_violin_-22to44axis.tiff", width = 5, height = 5)







########### Gamma Connectivity DICS (averaged across hemi) by entrainment condition - LH Lingual peak (WB LME)

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_DICS_LH_Lingual <- data[c(1:2,9:10)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_DICS_Hemi_LME_LH_Lingal_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_DICS_LH_Lingual), aes(x=FreqCond_AcrossHemi, y=Gamma_DICS_Hemi_LME_LH_Lingal, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Connectivity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous 
  coord_cartesian(ylim = c(-40,75)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_DICS_IndAvg_AcrossHemi_WB-LME_LH_Lingual_byEntrainCond_BoxPlot_violin_-40to75axis.tiff", width = 5, height = 5)







########### Gamma Connectivity DICS (averaged across hemi) by entrainment condition - LH Inferior Parietal peak (WB LME)

fontSize = 20
violin_fill_alpha = .2

pnt_offset_from_center = .8985
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

data_acrossHemi_DICS_RH_Cuneus <- data[c(1:2,11:12)]

mean_line <- data %>% group_by(FreqCond_AcrossHemi) %>% summarize(average = mean(Gamma_DICS_Hemi_RH_Cuneus_mean)) %>% ungroup()

p <- ggplot(data = na.omit(data_acrossHemi_DICS_RH_Cuneus), aes(x=FreqCond_AcrossHemi, y=Gamma_DICS_Hemi_RH_Cuneus, fill=FreqCond_AcrossHemi)) + theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=Freq_Condition_LH)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=FreqCond_AcrossHemi, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_line(data=mean, mapping = aes(x=Freq_Condition_LH, y=mean(values), group=1, color="black"))
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=Freq_Condition_LH)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=FreqCond_AcrossHemi), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  scale_fill_manual(values=c("#20A4F3","#68D89B","#D56062")) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=Freq_Condition_LH,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Connectivity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) +
  #scale_y_continuous 
  coord_cartesian(ylim = c(-40,75)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_DICS_IndAvg_AcrossHemi_WB-LME_RH_Cuneus_byEntrainCond_BoxPlot_violin_-40to75axis.tiff", width = 5, height = 5)







