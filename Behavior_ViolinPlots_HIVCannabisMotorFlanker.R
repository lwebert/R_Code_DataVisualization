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

# group_1_color1 = "#E58A75"
# group_1_color2 = "#CF4727"
# group_2_color1 = "#EAD48E"
# group_2_color2 = "#DDB946"
# group_3_color1 = "#81BCD1"
# group_3_color2 = "#4198B5"
# group_4_color1 = "#D7E399"
# group_4_color2 = "#BED158"
  
Congruent_color = "#FF9B85"
Incongruent_color = "#FFD97D"

Control_nonusers_Color = "#084b83"
Control_users_Color = "#42bfdd"
HIV_nonusers_Color = "#bbe6e4"
HIV_users_Color = "#386641"

#scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641"))

#Congruent_color = "#ED7B84"
#Incongruent_color = "#823038"

# Control_nonusers_Color = "#4198B5"
# Control_users_Color = "#97DB4F"
# HIV_nonusers_Color = "#265869"
# HIV_users_Color = "#53871C"
#               
# group_3_color1 = "#81BCD1"
# group_3_color2 = "#4198B5"
# group_4_color1 = "#D7E399"
# group_4_color2 = "#BED158"

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



################################### TASK BEHAVIOR Plots ###################################



######################----- Avg RT by Group by Condition------###########################

data <- read_xlsx('Behavior.xlsx', sheet = "HIV_Cannabis", col_names = TRUE)
  
data$GroupCondition <- paste(data$Group,"_",data$Condition,sep = '')
      
x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")
      
  p <- ggplot(data, aes(x=Group, y=Average_RT, fill=GroupCondition)) +
      geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
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
      
ggsave("AvgRT_by_group_by_condition_CongIncong_LKW.tiff", width = 5, height = 5)




######################----- Flanker RT by Group ------###########################

data <- read_xlsx('Behavior.xlsx', sheet = "HIV_Cannabis_FlankRT", col_names = TRUE)

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")

p <- ggplot(data, aes(x=Group, y=Flanker_RT, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-120,250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("FlankerRT_by_group_LKW.tiff", width = 5, height = 5)








# ######################----- Flanker RT x non-using Controls, PWH (main effect HIV pt.1) ------###########################
# 
# data <- read_xlsx('Behavior.xlsx', sheet = "new_0-2_HIVeffect_FlankRT", col_names = TRUE)
# 
# #Change the names so that the plots are ordered properly (will be changed back in the plot settings)
# data$Group <- paste(data$Group)
# 
# x_axis_labels <- c("HIV- Nonusers", "HIV+ Nonusers")
# 
# p <- ggplot(data, aes(x=Group, y=Flanker_RT, fill=Group)) +
#   geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
#   scale_fill_manual(values=c("#084b83","#bbe6e4")) +
#   geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 0.75) +
#   
#   
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   
#   
#   theme(plot.title = element_blank()) + 
#   theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
#   
#   
#   
#   xlab("Participant Group") + 
#   theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
#   theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
#   theme(axis.title.x = element_blank()) + 
#   
#   
#   ylab("Flanker Reaction Time (ms)") +
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
#   #scale_y_continuous +
#   coord_cartesian(ylim = c(-120,250))+ #This allows you to use ylim with percentages
#   theme(legend.position = "none")  
# 
# #Increased the y-limits so a sig. bar could be added.
# 
# plot(p)
# 
# ggsave("FlankerRT_MainEffectHIV_0-2_LKW__-120_250.tiff", width = 5, height = 5)
# 
# 
# 
# 
# 
# 
# 
# 
# ######################----- Flanker RT x cannabis user Controls, PWH (main effect HIV pt.2) ------###########################
# 
# data <- read_xlsx('Behavior.xlsx', sheet = "new_1-3_HIVeffect_FlankRT", col_names = TRUE)
# 
# #Change the names so that the plots are ordered properly (will be changed back in the plot settings)
# data$Group <- paste(data$Group)
# 
# x_axis_labels <- c("HIV- Users", "HIV+ Users")
# 
# p <- ggplot(data, aes(x=Group, y=Flanker_RT, fill=Group)) +
#   geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
#   scale_fill_manual(values=c("#42bfdd", "#386641")) +
#   geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 0.75) +
#   
#   
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   
#   
#   theme(plot.title = element_blank()) + 
#   theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
#   
#   
#   
#   xlab("Participant Group") + 
#   theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
#   theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
#   theme(axis.title.x = element_blank()) + 
#   
#   
#   ylab("Flanker Reaction Time (ms)") +
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
#   #scale_y_continuous +
#   coord_cartesian(ylim = c(-120,250))+ #This allows you to use ylim with percentages
#   theme(legend.position = "none")  
# 
# #Increased the y-limits so a sig. bar could be added.
# 
# plot(p)
# 
# ggsave("FlankerRT_MainEffectHIV_1-3_LKW_-120_250.tiff", width = 5, height = 5)








######################-----NEW Avg RT by Condition by cannabis group ------###########################

data <- read_xlsx('Behavior.xlsx', sheet = "new_RTcond_Cannabis_HIV", col_names = TRUE)

data$GroupCondition <- paste(data$Cannabis_Group,"_",data$Condition,sep = '')

x_axis_labels <- c("Nonuser", "User")

p <- ggplot(data, aes(x=GroupCondition, y=RT_byCondition, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color, Congruent_color, Incongruent_color)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center), size = 0.75) +
  stat_summary(fun.y=mean, geom="line", aes(group=Cannabis_Group))  +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = 10,family="Arial")) +
  theme(axis.text.x= element_text(size = 10,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("AvgRT_by_condition_CongIncong_by_CannabisGroup_LKW_meanLines--new.tiff", width = 5, height = 5)








######################-----NEW Avg RT by Condition by HIV group ------###########################

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
  stat_summary(fun.y=mean, geom="line", aes(group=HIV_Group))  +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_blank()) + 
  theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
  
  xlab("Participant Group") + 
  theme(axis.title.x = element_text(size = 10,family="Arial")) +
  theme(axis.text.x= element_text(size = 10,family="Arial")) +
  theme(axis.title.x = element_blank()) + 
  
  ylab("Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("AvgRT_by_condition_CongIncong_by_HIVGroup_LKW_meanLines--new.tiff", width = 5, height = 5)












# ################# Raincloud Plots
# 
#####----- RT by Group by Condition------###########################
# data <- read_xlsx('Behavior.xlsx', sheet = 2, col_names = TRUE)
# data$GroupCondition <- paste(data$Group,"_",data$Condition,sep = '')
# 
# x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")
# 
# p <- ggplot(data, aes(x=Group, y=Average_RT, fill=GroupCondition)) + theme(legend.position="none") +
#   ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, aes(fill=GroupCondition)) + 
#   geom_boxplot(width = .1, outlier.shape = NA) +
#   gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .75, aes(fill=GroupCondition)) +
#   #scale_fill_manual(values=c("#72195A", "#9EC1A3")) +
#   #geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') + 
#   #  geom_boxplot(width=0.2) + 
#   #scale_color_manual(values=c("#4695C0","#103358")) +
#   #scale_fill_manual(values=c("#4695C0","#103358")) +
#   scale_color_manual(values=c(group_1_color1, group_1_color2, group_2_color1, group_2_color2, group_3_color1, group_3_color2, group_4_color1, group_4_color2)) +
#   scale_fill_manual(values=c(group_1_color1, group_1_color2, group_2_color1, group_2_color2, group_3_color1, group_3_color2, group_4_color1, group_4_color2)) +
#   #geom_jitter(width = 0.06) +
#   
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) +
#   
#   theme(plot.title = element_blank()) + 
#   theme(plot.title = element_text(size = fontSize, face="bold",family='Arial')) +
#   
#   xlab("Task Condition") + 
#   theme(axis.title.x = element_text(size = fontSize,family="Arial")) +
#   theme(axis.text.x= element_text(size = fontSize,family="Arial")) +
#   theme(axis.title.x = element_blank()) + 
#   
#   ylab("Accuracy (%)") +
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text.y= element_text(size = fontSize,family="Arial"))  
#   #scale_y_continuous +
#   #coord_cartesian(ylim = c(400,1000)) #This allows you to use ylim with percentages
# 
# #Increased the y-limits so a sig. bar could be added.
# 
# plot(p)
# 
# #ggsave("RT_by_Condition_Raincloud.tiff", width = 5, height = 5)








################## Violin Plots Power by Group ###################################



########-----Gamma LH M1 Oscillatory Maximum by groups -----#####

data <- read_xlsx('Behavior.xlsx', sheet = "Gamma_m1", col_names = TRUE)

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")

              
  p <- ggplot(data, aes(x=Group, y=Gamma_LH_Postcentral_Rel_Max, fill=Group)) +
    geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
    scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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

    ylab("Flanker Reaction Time (ms)") +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
      #scale_y_continuous +
      #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
      theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_M1_OscillMax_by_group_Violin_LKW.tiff", width = 5, height = 5)






########-----Gamma LH M1 Oscillatory Average by Cond -----#####

data <- read_xlsx('Behavior.xlsx', sheet = "Gamma_m1_Cond", col_names = TRUE)

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
#data$Group <- paste(data$Group)

x_axis_labels <- c("Congruent", "Incongruent")


p <- ggplot(data, aes(x=Condition, y=Gamma_LH_M1_OscillAvg_noOutliersbyGroup, fill=Condition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Condition), trim=FALSE, color='black') +
  scale_fill_manual(values=c(Congruent_color, Incongruent_color)) +
  geom_point(position = position_jitterdodge(jitter.width=0.03,dodge.width=pnt_offset_from_center), size = 1) +
  
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_M1_OscillAvg_by_Cond_Violin_LKW.tiff", width = 5, height = 5)





########-----Gamma LH M1 Extracted Peak Oscillatory Pseudo-t by Group -----#####

data <- read_xlsx('Behavior.xlsx', sheet = 'Extracted_Peaks_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")


p <- ggplot(data, aes(x=Group, y=Gamma_LH_M1_ExtractedPeak_AveragedAcrossCond, fill=Group)) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_M1_Oscill_pseudo-t_by_group_Violin_LKW.tiff", width = 5, height = 5)









################## Pseudo-t (WB mrm peaks) violin plots by group and condition #########


data <- read_xlsx('Behavior.xlsx', sheet = 'Extracted_Peaks_byCondition', col_names = TRUE)

fontSize = 20

violin_fill_alpha = .9

pnt_offset_from_center = .8985
jitter = .04

#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$GroupCondition <- paste(data$Group,"_",data$Condition,sep = '')

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")



#########---------Beta RH dPMC by Condition and Group-----#########

p <- ggplot(data, aes(x=Group, y=Beta_RH_dPMC_ExtractedPeak, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
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
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  
                
#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Beta_Right_dPMC_by_group_condition_LKW.tiff", width = 5, height = 5)





#########---------Gamma LH mCC by Condition and Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_mCC_ExtractedPeak, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
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
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_Left_mCC_by_group_condition_LKW.tiff", width = 5, height = 5)





#########---------Gamma RH preSMA by Condition and Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_RH_preSMA_ExtractedPeak, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
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
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_Right_preSMA_by_group_condition_LKW.tiff", width = 5, height = 5)











################### Pseudo-t flanker effect and Spontaneous (WB mrm peaks) by group ###########

data <- read_xlsx('Behavior.xlsx', sheet = 'Extracted_Peaks_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")



#########---------Beta RH dPMC flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Beta_RH_dPMC_FlankerEffect, fill=Group)) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Beta_RH_dPMC_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20.tiff", width = 5, height = 5)






#########---------Beta RH dPMC Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Beta_RH_dPMC_SpontAvg_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Beta_RH_dPMC_Spontaneous_by_group_Violin_LKW.tiff", width = 5, height = 5)






#########---------Gamma LH mCC flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_mCC_FlankerEffect, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_mCC_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20.tiff", width = 5, height = 5)






#########---------Gamma LH mCC Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_mCC_SpontAvg_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_mCC_Spontaneous_by_group_Violin_LKW.tiff", width = 5, height = 5)





#########---------Gamma RH preSMA flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_RH_preSMA_FlankerEffect, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_preSMA_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20.tiff", width = 5, height = 5)






#########---------Gamma RH preSMA Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_RH_preSMA_SpontAvg_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_preSMA_Spontaneous_by_group_Violin_LKW.tiff", width = 5, height = 5)






#########---------Gamma LH M1 Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_M1_Spont_noOutliersbyGroup, fill=Group_Label, na.omit(NA), na.rm=TRUE)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group_Label), trim=FALSE, color='black') +
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
  
  ylab("Flanker Reaction Time (ms)") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_M1_Spontaneous_by_group_Violin_LKW.tiff", width = 5, height = 5)











################### NEW------Pseudo-t flanker effect and Spontaneous (WB lme peaks) by group ###########

data <- read_xlsx('Behavior.xlsx', sheet = 'NEW_ExtractPeak_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")





#########---------Beta RH Paracentral flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Beta_RH_Paracentral_noOutlier__Flanker, fill=Group)) +
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
  
  ylab("Flanker Pseudo-t") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Beta_RH_Paracentral_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20--new.tiff", width = 5, height = 5)





#########---------Gamma RH cerebellum flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_RH_Cerebellum_noOutlier__Flanker, fill=Group)) +
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
  
  ylab("Flanker Pseudo-t") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_Cerebellum_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20--new.tiff", width = 5, height = 5)






#########---------Gamma LH cerebellum-1 flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_Cerebellum1_noOutlier__Flanker, fill=Group)) +
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
  
  ylab("Flanker Pseudo-t") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_Cerebellum-1_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20--new.tiff", width = 5, height = 5)





#########---------Gamma LH cerebellum-2 flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_LH_Cerebellum2_noOutlier__Flanker, fill=Group)) +
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
  
  ylab("Flanker Pseudo-t") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_Cerebellum-2_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20--new.tiff", width = 5, height = 5)








#########---------Gamma LH Parietal Occipital Sulcus (splenium) Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_Spont_LH_Pariet_Occ_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gmma_LH_ParitalOccip_Spontaneous_by_group_Violin_LKW--new.tiff", width = 5, height = 5)




#########---------Gamma RH Cerebellum Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_Spont_RH_Cerebellum_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_Cerebellum_Spontaneous_by_group_Violin_LKW--new.tiff", width = 5, height = 5)




#########---------Gamma LH Cerebellum-1 Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_Spont_LH_Cerebellum1_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_Cerebellum-1_Spontaneous_by_group_Violin_LKW--new.tiff", width = 5, height = 5)





#########---------Gamma LH Cerebellum-2 Spontaneous by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_Spont_LH_Cerebellum2_noOutliersbyGroup, fill=Group)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=Group), trim=FALSE, color='black') +
  scale_fill_manual(values=c("#084b83","#42bfdd","#bbe6e4","#386641")) +
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
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_LH_Cerebellum-2_Spontaneous_by_group_Violin_LKW--new.tiff", width = 5, height = 5)













################### NEW------(WB Group Effect) Gamma right cerebellum by group ###########

data <- read_xlsx('Behavior.xlsx', sheet = 'NEW_ExtractPeak_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)

x_axis_labels <- c("HIV- Nonusers", "HIV- Users", "HIV+ Nonusers", "HIV+ Users")





#########---------Gamma RH Cerebellum flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_WB_GroupEffect_RH_Cerebellum_AllCond_noOutliersByGroup, fill=Group)) +
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
  
  ylab("Flanker Pseudo-t") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_Cerebellum_(WB-GroupEffect)_FlankerEffect_pseudo-t_by_group_Violin_LKW_-20to20--new.tiff", width = 5, height = 5)





#########---------Gamma RH Cerebellum flanker pseudo-t by Group-----#########

p <- ggplot(data, aes(x=Group, y=Gamma_Spont_RH_Cerebellum_WB_GroupEffect_noOutliersbyGroup, fill=Group)) +
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
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y= element_text(size = fontSize,family="Arial")) + 
  #scale_y_continuous +
  #coord_cartesian(ylim = c(-20,20))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("Gamma_RH_Cerebellum_(WB-GroupEffect)_Spontaneous_by_group_Violin_LKW--new.tiff", width = 5, height = 5)












######################### Correlations with Behavior #############################

data <- read_xlsx('Behavior.xlsx', sheet = 'Extracted_Peaks_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)




#########---------Avg RT x Gamma LH mCC Spont-----#########

p1 <- ggplot(data, aes(x = Avg_Combined_RT, y = Gamma_LH_mCC_SpontAvg_noOutliersbyGroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("Average_RT_x_Gamma_LH_mCC_Spont_LKW.tiff", width = 5, height = 5)






#########---------Flanker RT x Gamma RH preSMA Spont-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_RH_preSMA_SpontAvg_noOutliersbyGroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  
  xlab("Flanker Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("Flanker_RT_x_Gamma_RH_preSMA_Spont_LKW.tiff", width = 5, height = 5)







#########---------CUDIT x Gamma LH mCC Spont-----#########

p1 <- ggplot(data, aes(x = CUDITTotalScore, y = Gamma_LH_mCC_SpontAvg_noOutliersbyGroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  
  xlab("CUDIT score") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(axis.title.y = element_blank()) +
  
  #########I included the legend so you know which group is which color for now
  #theme(legend.position="none") +
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0,80))


plot(p1)

ggsave("CUDIT_x_Gamma_LH_mCC_Spont_LKW.tiff", width = 5, height = 5)







#########---------CUDIT x Gamma RH preSMA flanker pseudo-t-----#########

p1 <- ggplot(data, aes(x = CUDITTotalScore, y = Gamma_RH_preSMA_FlankerEffect)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  
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

ggsave("CUDIT_x_Gamma_RH_preSMA_FlankerEffect_pseudo-t_LKW.tiff", width = 5, height = 5)







#########---------Avg RT x Gamma LH M1 (grand avg peak) Spont-----#########

p1 <- ggplot(data, aes(x = Avg_Combined_RT, y = Gamma_LH_M1_Spont_noOutliersbyGroup, color=Group)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("Average_RT_x_Gamma_LH_M1_Spont_LKW.tiff", width = 5, height = 5)













#########################NEW------ Correlations with Behavior #############################

data <- read_xlsx('RPlot_SpontCorrel_FlankerRT_forR1.xlsx', sheet = 'Sheet1', col_names = TRUE)

data$Group <- paste(data$Group)

#########---------Flanker RT x Spont Beta (Int peak) RH dPMC-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Beta_SPM_HIVCannabis_RH_dPMC_30_12_65_Spont_noOutl)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Beta_RH_dPMC_IntPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Gamma (Int peak) RH Insula-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_SPM_HIVCannabis_RH_AntInsual_34_4_5_Spont_noOut)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Gamma_RH_Insula_IntPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Gamma (Int peak) RH dlPFC-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_SPM_HIVCannabis_RH_dlPFC_50_15_37_Spont_noOut)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Gamma_RH_dlPFC_IntPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Gamma (Int peak) RH vPMC-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_SPM_HIVCannabis_RH_vPMC_62_12_33_Spont_noOut)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Gamma_RH_vPMC_IntPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Gamma (Int peak) LH Cerebellum-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_SPM_HIVCannabis_LH_cerebellum_18_48_34_Spont_noOut)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Gamma_LH_Cerebellum_IntPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Beta (Grand-avg peak) LH M1-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Beta_GrandAvg_LH_M1_Abs_Spont_noOutlier3SDgroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Beta_LH_M1_GrandAvgPeak.tiff", width = 5, height = 5)






#########---------Flanker RT x Spont Gamma (GrandAvg peak) LH M1-----#########

p1 <- ggplot(data, aes(x = FlankerEffectRT, y = Gamma_GrandAvg_LH_M1_Abs_Spont_noOutlier3SDgroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("FlankerRT_x_Spont_Gamma_LH_M1_GrandAvgPeak.tiff", width = 5, height = 5)















#########################OLD------ Correlations with Behavior #############################
data <- read_xlsx('Behavior.xlsx', sheet = 'NEW_ExtractPeak_byGroup', col_names = TRUE)

data$Group <- paste(data$Group)

#########---------Avg RT x Gamma LH mCC Spont-----#########

p1 <- ggplot(data, aes(x = Avg_Combined_RT, y = Gamma_Spont_RH_Cerebellum_noOutliersbyGroup)) +
  geom_point(size = 2, color= "#c37d92") +
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, linewidth = 2) +
  
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  theme(axis.title.x = element_blank()) +
  
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
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

plot(p1)

ggsave("Average_RT_x_Gamma_RH_Cerebellum_Spont_LKW--new.tiff", width = 5, height = 5)



