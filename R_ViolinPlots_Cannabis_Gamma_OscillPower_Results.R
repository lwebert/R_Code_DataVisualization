
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
#loadfonts(device="win")
#font_import()
#fonts()


setwd('D:\\SCAN_OneDotGamma\\Derivatives\\R\\1FIGURES_Cannabis_Analaysis')

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 1, col_names = TRUE)


#"#68d9a4" "#eb5b8b"
Color_Users = "#188D32"
Color_Nonusers = "#A4FFB8"

Color_Left = "#FFBC85"
Color_Right = "#F97000" #"#FFB070"

Low32_Left = "#6096BA"
Low32_Right = "#A3CEF1"
Mid40_Left = "#6096BA"
Mid40_Right = "#A3CEF1"
High48_Left = "#6096BA"
High48_Right = "#A3CEF1"

Color_Low32 = "#0C5F97" # "#0F77BD"
Color_Mid40 = "#42AAF0" # "#55B3F1"
Color_High48 ="#C6E6FA"  # "#A7D8F8"

fontSize = 20
violin_fill_alpha = .55

pnt_offset_from_center = .8985
jitter = .05








#################################OSCILLATORY POWER (log)#################################


########## ---- OSCILLATORY POWER (log) -- Entrainment Condition, Hemisphere

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 'Cond_Hemi', col_names = TRUE)

data$ConditionHemisphere <- paste(data$Condition,"_",data$Hemisphere,sep = '')
# mean_line <- data %>% group_by(ConditionHemisphere) %>% summarize(average = mean(Gamma_OscillAvg_500to1500ms_log)) %>% ungroup()
# data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'zHigh48Hz'))
# data_for_line <- data %>% group_by(Condition) %>% summarise(mean(Gamma_OscillAvg_500to1500ms_log))

p <- ggplot(data = data, aes(x=ConditionHemisphere, y=Gamma_OscillAvg_500to1500ms_log, fill=ConditionHemisphere)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionHemisphere), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_Left, Low32_Right, Mid40_Left, Mid40_Right, High48_Left, High48_Right)) +
  scale_fill_manual(values=c(Low32_Left, Low32_Right, Mid40_Left, Mid40_Right, High48_Left, High48_Right)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_Log_Hemi_by_Cond_boxviolin_.tiff", width = 6, height = 6)





########## ---- OSCILLATORY POWER (log) -- Main Effect Entrainment Condition

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 'Cond', col_names = TRUE)
data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'zHigh48Hz'))

x_axis_labels <- c("32Hz", "40Hz", "48Hz")



### Right V1

data_RH_Oscill <- data[c(1,4:5)]
data_RH_Oscill = na.omit(data_RH_Oscill)
mean_line <- data_RH_Oscill %>% group_by(Condition) %>% summarize(average = mean(Gamma_OscillAvg_RightV1_500to1500ms_log_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_RH_Oscill), aes(x=Condition, y=Gamma_OscillAvg_RightV1_500to1500ms_log_noOutlier3SD, fill=Condition)) + 
  #geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Condition, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black', position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  
  scale_color_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  scale_fill_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_RightV1_Log_MainEffectCond_boxviolin_.tiff", width = 6, height = 6)



### Left V1

data_LH_Oscill <- data[c(1,4,6)]
data_LH_Oscill = na.omit(data_LH_Oscill)
mean_line <- data_LH_Oscill %>% group_by(Condition) %>% summarize(average = mean(Gamma_OscillAvg_LeftV1_500to1500ms_log_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_LH_Oscill), aes(x=Condition, y=Gamma_OscillAvg_LeftV1_500to1500ms_log_noOutlier3SD, fill=Condition)) + 
  #geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Condition, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black', position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  
  scale_color_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  scale_fill_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_LeftV1_Log_MainEffectCond_boxviolin_.tiff", width = 6, height = 6)




### Collapsed Across Hemi - V1

data_Hemi_Oscill <- na.omit(data)
mean_line <- data_Hemi_Oscill %>% group_by(Condition) %>% summarize(average = mean(Gamma_OscillAvg_AvgHemiV1_500to1500ms_log_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_Hemi_Oscill), aes(x=Condition, y=Gamma_OscillAvg_AvgHemiV1_500to1500ms_log_noOutlier3SD, fill=Condition)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  
  geom_line(data=mean_line, mapping = aes(x=Condition, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +

  scale_color_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  scale_fill_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_AvgHemiV1_Log_MainEffectCond_boxviolin.tiff", width = 6, height = 6)










########## ---- OSCILLATORY POWER (log) -- Main Effect Hemisphere

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 'Hemi', col_names = TRUE)
data$Hemisphere <- factor(data$Hemisphere, levels = c('Left', 'Right'))

x_axis_labels <- c("Left", "Right")


### Collapsed Across Conditions

data_AvgConds_Oscill <- na.omit(data)
mean_line <- data_AvgConds_Oscill %>% group_by(Hemisphere) %>% summarize(average = mean(Gamma_V1_AvgConds_OscillAvg_500to1500ms_log_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_AvgConds_Oscill), aes(x=Hemisphere, y=Gamma_V1_AvgConds_OscillAvg_500to1500ms_log_noOutlier3SD, fill=Hemisphere)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=Hemisphere), trim=FALSE, color='black') +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  
  scale_color_manual(values=c(Color_Left, Color_Right)) +
  scale_fill_manual(values=c(Color_Left, Color_Right)) +
  
  geom_line(data=mean_line, mapping = aes(x=Hemisphere, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_Log_AvgCond_MainEffectHemi_boxviolin_newColors.tiff", width = 6, height = 6)














#################################OSCILLATORY POWER (not log for visualization - DON'T USE)#################################


########## ---- OSCILLATORY POWER (not log) -- Main Effect Entrainment Condition

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 'Cond_noLog', col_names = TRUE)
data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'zHigh48Hz'))

x_axis_labels <- c("32Hz", "40Hz", "48Hz")


### Collapsed Across Hemi - V1

data_Hemi_Oscill <- na.omit(data)
mean_line <- data_Hemi_Oscill %>% group_by(Condition) %>% summarize(average = mean(Gamma_OscillAvg_AvgHemiV1_500to1500ms_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_Hemi_Oscill), aes(x=Condition, y=Gamma_OscillAvg_AvgHemiV1_500to1500ms_noOutlier3SD, fill=Condition)) + 
  #geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Condition, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  #geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black', position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  
  scale_color_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  scale_fill_manual(values=c(Color_Low32, Color_Mid40, Color_High48)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_AvgHemiV1_NoLog_MainEffectCond_boxviolin_.tiff", width = 6, height = 6)






########## ---- OSCILLATORY POWER (not log) -- Main Effect Hemisphere

data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 'Hemi_noLog', col_names = TRUE)
data$Hemisphere <- factor(data$Hemisphere, levels = c('Left', 'Right'))

x_axis_labels <- c("Left", "Right")



### Collapsed Across Conditions

data_AvgConds_Oscill <- na.omit(data)
mean_line <- data_AvgConds_Oscill %>% group_by(Hemisphere) %>% summarize(average = mean(Gamma_V1_AvgConds_OscillAvg_500to1500ms_noOutlier3SD)) %>% ungroup()


p <- ggplot(data = na.omit(data_AvgConds_Oscill), aes(x=Hemisphere, y=Gamma_V1_AvgConds_OscillAvg_500to1500ms_noOutlier3SD, fill=Hemisphere)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL) +
  geom_line(data=mean_line, mapping = aes(x=Hemisphere, y=average, group=1), color="black") +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Hemisphere), trim=FALSE, color='black') +
  
  scale_color_manual(values=c(Color_Left, Color_Right)) +
  scale_fill_manual(values=c(Color_Left, Color_Right)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_Cannabis_OscillAvg_NoLog_AvgAcrossConds_MainEffectHemi_boxviolin_.tiff", width = 6, height = 6)




