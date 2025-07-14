
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


setwd('D:\\SCAN_OneDotGamma\\Derivatives\\R')

data <- read_xlsx('R_Plot_Gamma_by_entrainment_condition.xlsx', sheet = 2, col_names = TRUE)


##############################Original Data##########################################

########### Oscillatory Average Gamma by Hemisphere by Entrainment Condition

fontSize = 20
violin_fill_alpha = .8

pnt_offset_from_center = .8985
jitter = .04

Low32_color1 = "#D56062"
Low32_color2 = "#F5B700"
Mid40_color1 = "#D56062"
Mid40_color2 = "#F5B700"
High48_color1 = "#D56062"
High48_color2 = "#F5B700"

pnt_offset_from_center = .8985
jitter = .04

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$ConditionHemisphere <- paste(data$Condition,"_",data$Hemisphere,sep = '')

x_axis_labels <- c("Low32", "Mid40", "High48")


p <- ggplot(data, aes(x=Condition, y=Gamma_OscillatoryAverage, fill=ConditionHemisphere)) +
     geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black') +
     scale_fill_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
     geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
        
        
     theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +

     theme(plot.title = element_blank()) + 
     theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
        
    xlab("Cannabis Group") + 
      theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
      theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
      theme(axis.title.x = element_blank()) + 
        
    ylab("Reaction Time (ms)") +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
      #scale_y_continuous +
      #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
      theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_OscillAverage_Hemisphere_by_Condition.tiff", width = 5, height = 5)







########### Oscillatory Maximum Gamma by Hemisphere by Entrainment Condition

fontSize = 20
violin_fill_alpha = .8

pnt_offset_from_center = .8985
jitter = .04

Low32_color1 = "#D56062"
Low32_color2 = "#F5B700"
Mid40_color1 = "#D56062"
Mid40_color2 = "#F5B700"
High48_color1 = "#D56062"
High48_color2 = "#F5B700"

pnt_offset_from_center = .8985
jitter = .04

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$ConditionHemisphere <- paste(data$Condition,"_",data$Hemisphere,sep = '')

x_axis_labels <- c("Low32", "Mid40", "High48")


p <- ggplot(data, aes(x=Condition, y=Gamma_OscillatoryMaximum, fill=ConditionHemisphere)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center, size = 1)) +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Cannabis Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_OscillMaximum_Hemisphere_by_Condition.tiff", width = 5, height = 5)








#################################Box Plots with Violin Plots##################################


########### Oscillatory Average Gamma

fontSize = 10
violin_fill_alpha = .2

pnt_offset_from_center = 2
jitter = .04

x_axis_labels <- c("32Hz", "40Hz", "48Hz")

#data_OscillAvg <- data[c(1:4)]

data$ConditionHemisphere <- paste(data$Condition,"_",data$Hemisphere,sep = '')

mean_line <- data %>% group_by(ConditionHemisphere) %>% summarize(average = mean(Gamma_OscillatoryAverage)) %>% ungroup()


p <- ggplot(data = data, aes(x=ConditionHemisphere, y=Gamma_OscillatoryAverage, fill=ConditionHemisphere)) + 
  theme(legend.position="none") +
  #ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=ConditionHemisphere)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  #geom_line(data = data, mapping = aes(x=Hemisphere_Low32Hz, y=Gamma_OscillatoryAverage_Low32, group=1), color="black") +
  #geom_line(data = data, mapping = aes(x=Hemisphere_Mid40Hz, y=Gamma_OscillatoryAverage_Mid40, group=1), color="black") +
  #geom_line(data = data, mapping = aes(x=Hemisphere_High48Hz, y=Gamma_OscillatoryAverage_High48, group=1), color="black") +
  #geom_line(data=mean_line, mapping = aes(x=ConditionHemisphere, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=ConditionHemisphere)) +
  #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black') + 
  #geom_boxplot(width=0.2) + 
  scale_color_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  scale_fill_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  #geom_jitter(width = 0.06) +
  #geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  #geom_line(aes(group=URSI), position = position_dodge(jitter),linewidth = .1,alpha = .5) +
  #geom_point(aes(fill=ConditionHemisphere,group=URSI), position = position_dodge(jitter)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = 10, face="bold",family='Arial')) +
  
  xlab("Entrainment Condition") + 
  theme(axis.title.x = element_text(size = 10,family="Arial")) +
  theme(axis.text.x= element_text(size = 10,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Oscillatory Power (%)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = 10,family="Arial")) 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-3,12)) #This allows you to use ylim with percentages

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_OscillAverage_Hemisphere_by_Condition_boxviolin.tiff", width = 5, height = 5)









##############CORRECT PLOTS

data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'High48Hz'))
data_for_line <- data %>% group_by(Condition) %>% summarise(mean(Gamma_OscillatoryAverage))

p <- ggplot(data = data, aes(x=ConditionHemisphere, y=Gamma_OscillatoryAverage, fill=ConditionHemisphere)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  scale_fill_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  coord_cartesian(ylim = c(-3,12))


plot(p)

ggsave("Gamma_OscillAverage_Hemisphere_by_Condition_boxviolin__.tiff", width = 6, height = 6)






p <- ggplot(data = data, aes(x=ConditionHemisphere, y=Gamma_OscillatoryMaximum, fill=ConditionHemisphere)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  scale_fill_manual(values=c(Low32_color1, Low32_color2, Mid40_color1, Mid40_color2, High48_color1, High48_color2)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition))  +
  coord_cartesian(ylim = c(-3,12))


plot(p)

ggsave("Gamma_OscillMaximum_Hemisphere_by_Condition_boxviolin__.tiff", width = 6, height = 6)


