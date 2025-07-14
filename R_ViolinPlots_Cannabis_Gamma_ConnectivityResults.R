
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

# data <- read_xlsx('R_Plot_Cannabis_Gamma_Oscill_Hemi.xlsx', sheet = 1, col_names = TRUE)


Color_Users = "#188D32"   #"#188D32" "#36964B" "#54A064"
Color_Nonusers = "#A4FFB8"  #"#A8F0B7" "#A5DFB2" "#AFD5B6"
  
Low32_Nonuser = "#A4FFB8"
Low32_User = "#188D32"
Mid40_Nonuser = "#A4FFB8"
Mid40_User = "#188D32"
High48_Nonuser = "#A4FFB8"
High48_User = "#188D32"

Color_Left = "#F97000"
Color_Right = "#FFBC85" #"#FFB070"
  
Color_Low32 = "#0C5F97" # "#0F77BD"
Color_Mid40 = "#42AAF0" # "#55B3F1"
Color_High48 ="#C6E6FA"  # "#A7D8F8"

fontSize = 20
violin_fill_alpha = .55

pnt_offset_from_center = .8985
jitter = .04



#################################CONNECTIVITY -- new ANOVA analysis, USE THIS!#################################


########## ---- Connectivity Extracted Peaks -- Entrainment Condition * Group Interaction Peaks

data <- read_xlsx('R_Plot_Cannabis_Gamma_Connectivity.xlsx', sheet = 'CondGroup_ANOVA', col_names = TRUE)

data$ConditionGroup <- paste(data$Condition,"_",data$Participant_Label,sep = '')





###Left V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=LHV1_RHCerebellum, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)


ggsave("Gamma_DICS_ANOVA_CondxGroup_LeftV1_RightCerebellum_boxviolin_trim.tiff", width = 6, height = 6)




###Left V1-Left Pars Opercularis

p <- ggplot(data = data, aes(x=ConditionGroup, y=LHV1_LHParsOperc, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)


ggsave("Gamma_DICS_ANOVA_CondxGroup_LeftV1_LeftParsOperc_boxviolin_trim.tiff", width = 6, height = 6)




###Right V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=RHV1_RHCerebellum, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)


ggsave("Gamma_DICS_ANOVA_CondxGroup_RightV1_RightCerebellum_boxviolin_trim.tiff", width = 6, height = 6)




###Right V1-Left Pars Opercularis

p <- ggplot(data = data, aes(x=ConditionGroup, y=RHV1_LHParsOperc, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)


ggsave("Gamma_DICS_ANOVA_CondxGroup_RightV1_LeftParsOperc_boxviolin_trim.tiff", width = 6, height = 6)




###Right V1-Left Lateral Occipital Cortex

p <- ggplot(data = data, aes(x=ConditionGroup, y=RHV1_LHLatOccip, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)


ggsave("Gamma_DICS_ANOVA_CondxGroup_RightV1_LeftLatOccip_boxviolin_trim.tiff", width = 6, height = 6)
























#################################CONNECTIVITY -- Old LME analysis, DON'T USE#################################


########## ---- Connectivity Extracted Peaks -- Entrainment Condition * Group Interaction Peaks

data <- read_xlsx('R_Plot_Cannabis_Gamma_Connectivity.xlsx', sheet = 'Cond_Group', col_names = TRUE)

data$ConditionGroup <- paste(data$Condition,"_",data$Participant_Label,sep = '')
# mean_line <- data %>% group_by(ConditionHemisphere) %>% summarize(average = mean(Gamma_OscillAvg_500to1500ms_log)) %>% ungroup()
# data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'zHigh48Hz'))
# data_for_line <- data %>% group_by(Condition) %>% summarise(mean(Gamma_OscillAvg_500to1500ms_log))




###Right V1-Left IFG

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_RightV1_LeftIFG, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_RightV1_LeftIFG_CondxGroup_boxviolin_trim.tiff", width = 6, height = 6)





###Right V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_RightV1_RightCerebellum, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_RightV1_RightCerebellum_CondxGroup_boxviolin_trim.tiff", width = 6, height = 6)





###Left V1-Left IFG

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_LeftV1_LeftIFG, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_LeftV1_LeftIFG_CondxGroup_boxviolin_trim.tiff", width = 6, height = 6)





###Left V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_LeftV1_RightCerebellum, fill=ConditionGroup)) + 
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=TRUE, color='black', position=position_dodge(1)) +
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_LeftV1_RightCerebellum_CondxGroup_boxviolin_trim.tiff", width = 6, height = 6)












#################################CONNECTIVITY - peak outliers removed -- DON'T USE#################################

data <- read_xlsx('R_Plot_Cannabis_Gamma_Connectivity.xlsx', sheet = 'Cond_Group_noOut', col_names = TRUE)

data$ConditionGroup <- paste(data$Condition,"_",data$Participant_Label,sep = '')
# mean_line <- data %>% group_by(ConditionHemisphere) %>% summarize(average = mean(Gamma_OscillAvg_500to1500ms_log)) %>% ungroup()
# data$Condition <- factor(data$Condition, levels = c('Low32Hz', 'Mid40Hz', 'zHigh48Hz'))
# data_for_line <- data %>% group_by(Condition) %>% summarise(mean(Gamma_OscillAvg_500to1500ms_log))




###Right V1-Left IFG

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_RightV1_LeftIFG_noOut, fill=ConditionGroup)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_RightV1_LeftIFG_CondxGroup_boxviolin_nooutlier.tiff", width = 6, height = 6)





###Right V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_RightV1_RightCerebellum_noOut, fill=ConditionGroup)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_RightV1_RightCerebellum_CondxGroup_boxviolin_nooutlier.tiff", width = 6, height = 6)





###Left V1-Left IFG

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_LeftV1_LeftIFG_noOut, fill=ConditionGroup)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_LeftV1_LeftIFG_CondxGroup_boxviolin_nooutlier.tiff", width = 6, height = 6)





###Left V1-Right Cerebellum

p <- ggplot(data = data, aes(x=ConditionGroup, y=Gamma_Connectivity_LeftV1_RightCerebellum_noOut, fill=ConditionGroup)) + 
  geom_boxplot(width = .15, outlier.shape = NA, fatten = NULL, position=position_dodge(1)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=ConditionGroup), trim=FALSE, color='black', position=position_dodge(1)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_blank()) +
  scale_color_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  scale_fill_manual(values=c(Low32_Nonuser, Low32_User, Mid40_Nonuser, Mid40_User, High48_Nonuser, High48_User)) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=Condition)) +
  #coord_cartesian(ylim = c(-3,12)) + 
  theme(aspect.ratio = 1)

plot(p)
ggsave("Gamma_DICS_Connectivity_LeftV1_RightCerebellum_CondxGroup_boxviolin_nooutlier.tiff", width = 6, height = 6)

