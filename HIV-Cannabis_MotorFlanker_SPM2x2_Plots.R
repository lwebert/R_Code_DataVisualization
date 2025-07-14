setwd('D:\\Flanker_HIV_Cannabis\\Derivatives\\R\\HIV-Cannabis_motor_flanker')

library(readxl)
library(ggplot2)
library(uniReg)
library(ggimage)

axis_text_text_size = 15
axis_title_text_size = 20

best_fit_line_color = "#000000"
best_fit_SEM_color  = "#DBDBDB"
    
dot_Color       = "#000000"
dot_size            = 0.5

Congruent_color = "#FF9B85"
Incongruent_color = "#FFD97D"

Control_nonusers_Color = "#084b83"
Control_users_Color = "#42bfdd"
HIV_nonusers_Color = "#bbe6e4"
HIV_users_Color = "#386641"


Control_Color = "#229FBF"
HIV_Color = "#85D6EA"
Nonuser_Color = "#96C59E"
User_Color = "#4F925D"

library(readxl)
library(ggplot2)

#To get extra fonts for plotting, load:
library(extrafont)

#fonts
loadfonts(device="win")
font_import()
fonts()

fontSize = 20

violin_fill_alpha = .9

pnt_offset_from_center = .8985
jitter = .04



################### !! Behavior !! ###########

######################----- Avg RT by Condition------###########################

#data <- read_xlsx('Behavior.xlsx', sheet = "HIV_Cannabis", col_names = TRUE)
#data$GroupCondition <- paste(data$Group,"_",data$Condition,sep = '')
#x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")


data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'Sheet2')
#data$Group <- paste(data$Group)


p <- ggplot(data, aes(x=Condition, y=AvgRT_byCond, fill=Condition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 0.75) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("AvgRT_by_condition_allParticipants_LKW_200-1250.tiff", width = 5, height = 5)


######################----- Avg RT by Condition by HIV group ------###########################

data <- read_xlsx('Behavior.xlsx', sheet = "new_RTcond_Cannabis_HIV", col_names = TRUE)

#data$meanHIVGroups <- factor(data$HIV_Group, levels = c('HIV-', 'HIV+'))
data$GroupCondition <- paste(data$HIV_Group,"_",data$Condition,sep = '')

x_axis_labels <- c("HIV-", "HIV+")

p <- ggplot(data, aes(x=GroupCondition, y=RT_byCondition, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black', position = position_dodge(1)) +
  scale_fill_manual(values=c(Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter, dodge.width=1), size = 0.75) +
  #add trend lines between means: need the below stat_summary command and x=GroupCondition in ggplot
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black", aes(group=GroupCondition), position = position_dodge(1)) +
  #stat_summary(fun.y=mean, geom="line", aes(group=HIV_Group))  +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 

  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize, family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize, family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")

plot(p)

ggsave("AvgRT_by_condition_by_HIVGroup_LKW_200-1250.tiff", width = 5, height = 5)





######################----- Flanker RT by HIV status ------###########################

data <- read_xlsx('Behavior.xlsx', sheet = "FlankerRT_HIVeffect", col_names = TRUE)

#data$meanHIVGroups <- factor(data$HIV_Group, levels = c('HIV-', 'HIV+'))

x_axis_labels <- c("HIV-", "HIV+")

p <- ggplot(data, aes(x=factor(HIV), y=Flanker_RT, fill=factor(HIV))) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=factor(HIV)), trim=FALSE, color='black', position = position_dodge(1)) +
  scale_fill_manual(values=c(Control_Color, HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter, dodge.width=1), size = 0.75) +
  #add trend lines between means: need the below stat_summary command and x=GroupCondition in ggplot
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2.5, color="black", fill="black", aes(group=GroupCondition), position = position_dodge(1)) +
  #stat_summary(fun.y=mean, geom="line", aes(group=HIV_Group))  +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("HIV Status") + 
  theme(axis.title.x = element_text(size = fontSize, family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize, family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")

plot(p)

ggsave("FlankerRT_by_HIVStatus_Violins.tiff", width = 5, height = 5)












################### !! SPM 2x2 interaction peaks (flanker pseudo-t) by group !! ###########

data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'Sheet1')
data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")




#########---------Beta RH dPMC flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Beta_FlankerPeuedot_SPM_HIVCannabis_RH_dPMC_noOutlierbyGroup3SD, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_dPMC_(SPM2x2)_Flanker_Pseudo-t_by_group_Violin.tiff", width = 5, height = 5)






#########---------Gamma LH Cerebellum flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_FlankerPeuedot_SPM_HIVCannabis_LH_cerebellum_noOutlierbyGroup3SD, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_Cerebellum_(SPM2x2)_Flanker_Pseudo-t_by_group_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH Insula flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_FlankerPeuedot_SPM_HIVCannabis_RH_Insual_noOutlierbyGroup3SD, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_Insula_(SPM2x2)_Flanker_Pseudo-t_by_group_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH dlPFC flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_FlankerPeuedot_SPM_HIVCannabis_RH_dlPFC_noOutlierbyGroup3SD, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_dlPFC_(SPM2x2)_Flanker_Pseudo-t_by_group_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH vPMC flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_FlankerPeuedot_SPM_HIVCannabis_RH_vPMC_noOutlierbyGroup3SD, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_vPMC_(SPM2x2)_Flanker_Pseudo-t_by_group_Violin.tiff", width = 5, height = 5)












################### !! SPM 2x2 interaction peaks (Spontaneous time series) by group !! ###########

data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'SpontSPMInt')
data$Group <- paste(data$Group)
data$Cannabis <- paste(data$Cannabis)
data$HIV <- paste(data$HIV)



Control_Color = "#229FBF"
HIV_Color = "#85D6EA"
Nonuser_Color = "#96C59E"
User_Color = "#4F925D"






#########---------Beta RH dPMC spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Beta_SPM_RH_dPMC_Spont_noOutliersbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 

  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_dPMC_(SPM2x2)_Spontaneous_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Beta RH dPMC spontaneous main effect HIV-----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Beta_SPM_RH_dPMC_Spont_noOutliersbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_dPMC_(SPM2x2)_Spontaneous_HIVEffect_Violin.tiff", width = 5, height = 5)








#########---------Gamma RH vPMC spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SPM_RH_vPMC_Spont_noOutliersbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_vPMC_(SPM2x2)_Spontaneous_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH vPMC spontaneous main effect HIV-----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SPM_RH_vPMC_Spont_noOutliersbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_vPMC_(SPM2x2)_Spontaneous_HIVEffect_Violin.tiff", width = 5, height = 5)









#########---------Gamma RH Insula spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SPM_RH_AntInsual_Spont_noOutliersbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_Insula_(SPM2x2)_Spontaneous_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH Insula spontaneous main effect HIV-----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SPM_RH_AntInsual_Spont_noOutliersbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_Insula_(SPM2x2)_Spontaneous_HIVEffect_Violin.tiff", width = 5, height = 5)










#########---------Gamma RH dlPFC spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SPM_RH_dlPFC_Spont_noOutliersbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_dlPFC_(SPM2x2)_Spontaneous_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH dlPFC spontaneous main effect HIV-----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SPM_RH_dlPFC_Spont_noOutliersbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_dlPFC_(SPM2x2)_Spontaneous_HIVEffect_Violin.tiff", width = 5, height = 5)










#########---------Gamma LH Cerebellum spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SPM_LH_cerebellum_Spont_noOutliersbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_Cerebellum_(SPM2x2)_Spontaneous_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Gamma LH Cerebellum spontaneous main effect HIV-----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SPM_LH_cerebellum_Spont_noOutliersbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-25,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_Cerebellum_(SPM2x2)_Spontaneous_HIVEffect_Violin.tiff", width = 5, height = 5)














################### !! Grand Average Left M1 (Spontaneous time series) by group !! ###########

data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'SpontM1')
data$Group <- paste(data$Group)
data$Cannabis <- paste(data$Cannabis)
data$HIV <- paste(data$HIV)



Control_Color = "#229FBF"
HIV_Color = "#85D6EA"
Nonuser_Color = "#96C59E"
User_Color = "#4F925D"





#########---------Gamma LH M1 spontaneous main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_GrandAvg_LH_M1_Abs_Spont_noOutlier3SDgroup, fill=Cannabis)) +
        geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
        #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
        scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
        geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
        
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        
        theme(plot.title = element_blank()) + 
        
        xlab("Participant Group") + 
        #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
        theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
        theme(axis.title.x = element_blank()) + 
        
        ylab(expression ("Flanker Pseudo-t")) + 
        theme(axis.title.y = element_blank()) +
        theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
        #scale_y_continuous +
        coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
        theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_M1_(GrandAvg)_Spontaneous_CannabisEffect_Violin_-5to25.tiff", width = 5, height = 5)




#########---------Gamma LH M1 spontaneous main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_GrandAvg_LH_M1_Abs_Spont_noOutlier3SDgroup, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_M1_(GrandAvg)_Spontaneous_HIVEffect_Violin_-5to25.tiff", width = 5, height = 5)
















################### !! SPM Main effect Cannabis (Oscillatory Pseudo-t) !! ###########

data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'MainEffectOscill')
data$Group <- paste(data$Group)
data$Cannabis <- paste(data$Cannabis)
data$HIV <- paste(data$HIV)


#########---------Gamma RH Cerebellum main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SubMap_SPM_CannabisEffect_RH_cerebellum_18_48_54_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_Cerebellum_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)




#########---------Gamma RH dmPFC main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SubMap_SPM_Cannabiseffect_RH_dmPFC_22_31_53_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_dmPFC_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)






#########---------Gamma RH Superior Parietal main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SubMap_SPM_Cannabiseffect_RH_SupParietal_26_60_57_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_SupParietal_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)







#########---------Gamma RH vPMC main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Gamma_SubMap_SPM_Cannabiseffect_RH_vPMC_62_0_21_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_vPMC_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)






#########---------Beta RH Inferior Parietal main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Beta_SubMap_SPM_CannabisEffect_RH_InfParietal_34_44_57_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_InfParietal_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)






#########---------Beta RH Orbital Frontal main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Beta_SubMap_SPM_CannabisEffect_RH_OrbitoFrontal_14_15_18_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_OrbitoFrontal_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)






#########---------Beta RH Paracentral main effect cannabis use-----#########

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=Cannabis, y=Beta_SubMap_SPM_CannabisEffect_RH_paracentral_2_44_61_noOutlierbyGroup3SD, fill=Cannabis)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Cannabis), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Nonuser_Color,User_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Beta_RH_Paracentral_SPM_CannabisEffect_Violin.tiff", width = 5, height = 5)

















################### !! SPM Main effect HIV (Oscillatory Pseudo-t) !! ###########

data <- read_xlsx('HIV_Cannabis_SPM_2x2_Plots.xlsx', sheet = 'MainEffectOscill')
data$Group <- paste(data$Group)
data$Cannabis <- paste(data$Cannabis)
data$HIV <- paste(data$HIV)



#########---------Gamma LH SMA main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SubMap_SPM_HIVeffect_LH_SMA_14_11_45_noOutlierbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_SMA_SPM_HIVEffect_Violin.tiff", width = 5, height = 5)





#########---------Gamma LH Thalamus main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SubMap_SPM_HIVeffect_LH_thalamus_14_24_5_noOutlierbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_Thalamus_SPM_HIVEffect_Violin.tiff", width = 5, height = 5)





#########---------Gamma RH Cerebellum main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Gamma_SubMap_SPM_HIVeffect_RH_cerebellum_10_84_34_noOutlierbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_RH_Cerebellum_SPM_HIVEffect_Violin.tiff", width = 5, height = 5)






#########---------Beta LH dlPMC main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Beta_SubMap_SPM_HIVEffect_LH_dlPMC_50_19_37_noOutlierbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_dlPMC_SPM_HIVEffect_Violin.tiff", width = 5, height = 5)







#########---------Beta LH dmPMC main effect HIV -----#########

x_axis_labels <- c("HIV-", "HIV")

p <- ggplot(data, aes(x=HIV, y=Beta_SubMap_SPM_HIVEffect_LH_dmPMC_22_19_53_noOutlierbyGroup3SD, fill=HIV)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=HIV), trim=FALSE, color='black') +
  #scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
  scale_fill_manual(values=c(Control_Color,HIV_Color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 1) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  
  xlab("Participant Group") + 
  #theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
  theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab(expression ("Flanker Pseudo-t")) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-5,25))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

plot(p)

ggsave("Gamma_LH_dmPMC_SPM_HIVEffect_Violin.tiff", width = 5, height = 5)














################### !! CORRELATION PLOTS !! ################### 


##### CUDIT x oscillatory (flanker pseudo-t) gamma RH dlPFC (SPM 2x2 peak) ######

p1 <- ggplot(data, aes(x = CUDITTotalScore, y = Gamma_FlankerPeuedot_SPM_HIVCannabis_RH_dlPFC_noOutlierbyGroup3SD)) +
  geom_point(size = 3, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 1.5) +
  
  xlab("CUDIT score") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Flanker Effect pseudo-t")) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(axis.title.y = element_blank()) +
  
  #########I included the legend so you know which group is which color for now
  #theme(legend.position="none") +
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) 
  #coord_cartesian(ylim = c(-12,8))


plot(p1)

ggsave("CUDIT_x_Gamma_RH_dlPFC_(SPM2x2)_FlankerEffect_pseudo-t.tiff", width = 5, height = 5)



##### CUDIT x oscillatory (flanker pseudo-t) gamma RH dlPFC (SPM 2x2 peak) by HIV-status ######

p1 <- ggplot(data, aes(x = CUDITTotalScore, y = Gamma_FlankerPeuedot_SPM_HIVCannabis_RH_dlPFC_noOutlierbyGroup3SD, color = HIV_Status)) +
  geom_smooth(aes(group=HIV_Status), method=lm, fill=best_fit_SEM_color, se=TRUE, size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#42bfdd", "#386641")) +
  
  xlab("CUDIT score") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Flanker Effect pseudo-t")) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(axis.title.y = element_blank()) +
  
  #########I included the legend so you know which group is which color for now
  #theme(legend.position="none") +
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) 
#coord_cartesian(ylim = c(-12,8))


plot(p1)

ggsave("CUDIT_x_Gamma_RH_dlPFC_(SPM2x2)_FlankerEffect_pseudo-t_byHIV.tiff", width = 5, height = 5)







