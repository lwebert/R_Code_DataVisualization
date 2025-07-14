#################################Scatter Plots##################################
setwd('D:/Flanker_HIV_Cannabis/Derivatives/R')

library(readxl)
library(ggplot2)
library(uniReg)
library(ggimage)

axis_text_text_size = 15
axis_title_text_size = 20

best_fit_line_color = "#000000"
best_fit_SEM_color  = "#DBDBDB"

Color1 = "#20A4F3"
Color2 = "#68D89B"

dot_Color       = "#9F007A"
dot_size            = 2.5

data <- read_xlsx('Motor_Cannabis_Excel.xlsx', sheet = 4, col_names = TRUE)

data2 <- data[order(data$AGE),]

data2$Img = data2$Group
data2$Img[data2$Img=="Control/Nonuser"] = 'Controls.png'
data2$Img[data2$Img=="Control/User"] = 'cannabis_new.png'

data2$GroupSize <- with(data2, ifelse(Group == "Control/Nonuser", 0.02, 0.06))

######################NEW Low Gamma with RT########################
x   = data2$Avg_Combined
y   = data2$New_LowGamma_All_AbsSpont


############ORIGINAL
p1 <- ggplot(data2, aes(x = x, y = y)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  scale_colour_gradient(low = "#9EC1A3", high = "#70A9A1") +
  geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim=c(0, 24), xlim = c(450,1050))


plot(p1)

ggsave("Scatterplot_NEWLowGamma_Spontaneous_with_RT_Overall.tiff", width = 5, height = 5)



##############OVERALL - everything on it
p1 <- ggplot(data2, aes(x = x, y = y, color = Group)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  scale_color_manual(values = c(Color1, Color2)) +
  geom_smooth(aes(group = Dummy_Group), method=lm, se=FALSE, size = 1.5) +
  geom_smooth(method=lm, color = best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 1.5) +
  new_scale_colour() +
  #scale_colour_gradientn(colors = c("purple","orange")) +
  scale_colour_gradientn(colors = c("#9EC1A3","#70A9A1")) +
  geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.line = element_line(colour = "black"))
plot(p1)

ggsave("Scatterplot_NEWLowGamma_Spontaneous_with_RT_byGroup_andOverall.tiff", width = 5, height = 5)




#############just the 2 lines (by group)
p1 <- ggplot(data2, aes(x = x, y = y, color = Group)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  scale_color_manual(values = c(Color1, Color2)) +
  geom_smooth(aes(group = Group), method=lm, se=FALSE, size = 1.5) +
  #geom_smooth(method=lm, color = best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 1.5) +
  #geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim=c(0, 24), xlim = c(450,1050))

plot(p1)

ggsave("Scatterplot_NEWLowGamma_Spontaneous_with_RT_LinesbyGroup.tiff", width = 5, height = 5)



######################Low Gamma with RT########################
x   = data2$Avg_Combined
y   = data2$LowGamma_LH_Postcentral_Abs_Spont

p1 <- ggplot(data2, aes(x = x, y = y)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  scale_colour_gradient(low = "#9EC1A3", high = "#70A9A1") +
  geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
plot(p1)
ggsave("Scatterplot_LowGamma_Spontaneous_with_RT.tiff", width = 5, height = 5)




#################################Scatter Plots##################################
setwd('D:/Flanker_HIV_Cannabis/Derivatives/R')

library(readxl)
library(ggplot2)
library(uniReg)
library(ggimage)

axis_text_text_size = 15
axis_title_text_size = 20

best_fit_line_color = "#000000"
best_fit_SEM_color  = "#DBDBDB"

dot_Color       = "#9F007A"
dot_size            = 2.5

data <- read_xlsx('Motor_Cannabis_Excel.xlsx', sheet = 3, col_names = TRUE)

data2 <- data[order(data$AGE),]

data2$Img = data2$Group
data2$Img[data2$Img=="Nonusers"] = 'Controls.png'
data2$Img[data2$Img=="Users"] = 'cannabis_new.png'

data2$GroupSize <- with(data2, ifelse(Group == "Nonusers", 0.02, 0.06))


######################High Gamma with RT########################
x   = data2$Avg_Combined
y   = data2$HighGamma_LH_Post_Abs_Spont

p1 <- ggplot(data2, aes(x = x, y = y)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  scale_colour_gradient(low = "#9EC1A3", high = "#70A9A1") +
  geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab("Reaction Time (ms)") +
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab(expression ("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
plot(p1)
ggsave("Scatterplot_HighGamma_Spontaneous_with_RT.tiff", width = 5, height = 5)


######################Low Gamma Spontaneous with Oscillatory########################
x   = data2$LowGamma_LH_Postcentral_Abs_Spont
y   = data2$LowGamma_LH_Post_Rel_Max

p1 <- ggplot(data2, aes(x = x, y = y)) +
  #geom_point(color = dot_Color, size = dot_size) +
  #geom_point() +
  # geom_image(aes(image=Img), size=data2$Age/500)+
  geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
  scale_colour_gradient(low = "#9EC1A3", high = "#70A9A1") +
  geom_image(aes(image=Img), size = data2$GroupSize)+
  xlab(expression("Spontaneous Power "~(nAm^2))) + 
  theme(axis.title.x = element_text(size = axis_title_text_size)) +
  theme(axis.text.x = element_text(size = axis_text_text_size)) +
  ylab("Oscillatory Power (%)") + 
  theme(axis.title.y = element_text(size = axis_title_text_size)) +
  theme(axis.text.y= element_text(size = axis_text_text_size))+
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
plot(p1)
ggsave("Scatterplot_LowGamma_Spontaneous_with_Oscillatory.tiff", width = 5, height = 5)

#################################Violin Plots###################################

######################-----RT by Cannabis Group------###########################
setwd('D:/Flanker_HIV_Cannabis/Derivatives/R')

library(readxl)
library(ggplot2)

#To get extra fonts for plotting, load:
library(extrafont)

#fonts
loadfonts(device="win")
font_import()
fonts()

# create a df

data <- read_xlsx('Behavior.xlsx')

fontSize = 20

violin_fill_alpha = .75


group_1_color1 = "#9D1B46"
group_1_color2 = "#D86F9A"
group_2_color1 = "#9D1B46"
group_2_color2 = "#D86F9A"

pnt_offset_from_center = .8985
jitter = .04
#Change the names so that the plots are ordered properly (will be changed back in the plot settings)
data$GroupCondition <- paste(data$Group,"_",data$Condition,sep = '')

x_axis_labels <- c("Cannabis Users", "Nonusers")




p <- ggplot(data, aes(x=Group, y=RT, fill=GroupCondition)) +
  geom_violin(alpha = violin_fill_alpha, aes(fill=GroupCondition), trim=FALSE, color='black') +
  scale_fill_manual(values=c(group_1_color1, group_1_color2, group_2_color1, group_2_color2)) +
  geom_point(position = position_jitterdodge(jitter.width=jitter,dodge.width=pnt_offset_from_center)) +
  
  
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
  coord_cartesian(ylim = c(200,1250))+ #This allows you to use ylim with percentages
  theme(legend.position = "none")  

#Increased the y-limits so a sig. bar could be added.

plot(p)

ggsave("RT_CannabisUsers_vs_Nonusers.tiff", width = 5, height = 5)




############################Half Violin Plots###################################

################################Spontaneous#####################################



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

# width and height variables for saved plots
w = 6
h = 4
# Define limits of y-axis
y_lim_min = 4
y_lim_max = 7.5


Color1 = "#20A4F3"
Color2 = "#68D89B"


setwd('E:\\Projects\\AUX_script\\ScatterImagePlot_R')
# setwd('D:\\Flanker_HIV_Cannabis\\Derivatives\\R')

# data <- read_excel("D:\\Flanker_HIV_Cannabis\\Derivatives\\R\\High_Low_Gamma_Spontaneous_Oscillatory.xlsx",
#                    sheet = "Sheet1")

data <- read_excel("E:\\Projects\\AUX_script\\ScatterImagePlot_R\\High_Low_Gamma_Spontaneous_Oscillatory.xlsx",
                                                       sheet = "Sheet1")

###------------Saving the plot

OutFolder = "E:\\Projects\\AUX_script\\ScatterImagePlot_R\\Figures\\"
# OutFolder = "D:\\Flanker_HIV_Cannabis\\Derivatives\\R\\Figures\\"
OutFileName = "HighLowGammaSpontaneous.tif"

Out <- paste(OutFolder, OutFileName, sep="")


###############################################
########-----------------Spontaneous-------------

Img1 = 'Controls.png'
Img2 = 'cannabis_new.png'
Img1sz = 0.01
Img2sz = 0.03

tmpLowGamm <- filter(data, Gamma_Band == "Low Gamma")
LowGammas = tmpLowGamm$Spontaneous
LowGammasColor <- with(tmpLowGamm, ifelse(Group == "Nonusers", Color1, Color2))
LowGammaImgs <- with(tmpLowGamm, ifelse(Group == "Nonusers", Img1, Img2))
LowGammaImgsSz <- with(tmpLowGamm, ifelse(Group == "Nonusers", Img1sz, Img2sz))


tmpHighGamm <- filter(data, Gamma_Band == "High Gamma")
HighGammas = tmpHighGamm$Spontaneous
HighGammasColor <- with(tmpHighGamm, ifelse(Group == "Nonusers", Color1, Color2))
HighGammasImgs <- with(tmpHighGamm, ifelse(Group == "Nonusers", Img1, Img2))
HighGammasImgsSz <- with(tmpHighGamm, ifelse(Group == "Nonusers", Img1sz, Img2sz))

DotSize = 1.5

n <- length(LowGammas) 
d <- data.frame(y = c(LowGammas, HighGammas),
                colr = c(LowGammasColor, HighGammasColor),
                Images = c(LowGammaImgs, HighGammasImgs),
                ImageSizes = c(LowGammaImgsSz, HighGammasImgsSz),
                x = rep(c(1,2), each=n),
                id = factor(rep(1:n,2)))

Color1 = "#20A4F3"
Color2 = "#68D89B"
# 
# d$Img = d$x
# d$Img[d$Img==1] = 'Controls.png'
# d$Img[d$Img==2] = 'cannabis_new.png'

set.seed(321)
d$xj <- jitter(d$x, amount = .09) 
#d$xj_2 <- jitter(d$z, amount = .09)

Pal <- c(Color1, Color2)

par(mar=c(7,7,6,2.1))

p1 <- ggplot(data=d, aes(y=y, color=colr)) +
  
  #Add geom_() objects
  # geom_point(data = d %>% filter(colr==Color1), aes(x=xj), size = DotSize,
  #            alpha = 1, color = Color1) +
  # 
  # geom_point(data = d %>% filter(colr==Color2), aes(x=xj), size = DotSize,
  #            alpha = 1, color = Color2) +
  # 
  geom_line(aes(x=xj, group=id, colour = colr), alpha = 1) +
  
  geom_image(aes(image=Images, x=xj),size = d$ImageSizes)+
  
  geom_half_violin(
    data = d %>% filter(x=="1" & colr==Color1),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = Color1, alpha = .5, color = Color1, trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="1" & colr==Color2),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = Color2, alpha = .5, color = Color2, trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="2" & colr==Color1),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = Color1, alpha = .5, color = Color1, trim = TRUE) +
  geom_half_violin(
    data = d %>% filter(x=="2" & colr==Color2),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = Color2, alpha = .5, color = Color2, trim = TRUE) +
  
  #Define additional settings
  scale_x_continuous(breaks=c(1,2), labels=c("Low Gamma", "High Gamma"), limits=c(0.1, 2.9)) +
  #  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c("Before", "During", "After","Follow", "Before", "During", "After", "Follow"), limits=c(0, 9)) +
  xlab("Frequency Range") + ylab(expression("Power " ~(nAm^2))) + ggtitle("Spontaneous") + 
  #ggtitle('Figure 13: Repeated measures with jittered datapoints and connections') +
  scale_color_manual(values=Pal) + 
  theme_classic() +
  theme(legend.position = "none") + 
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(ylim=c(0, 25))

plot(p1)
ggsave("ViolinPlot_LowHighGamma_Spontaneous.tiff", width = 5, height = 5)


###############################################
########-----------------Oscillatory-------------
Img1 = 'Controls.png'
Img2 = 'cannabis_new.png'
Img1sz = 0.01
Img2sz = 0.03

tmpLowGamm <- filter(data, Gamma_Band == "Low Gamma")
LowGammas = tmpLowGamm$Oscillatory
LowGammasColor <- with(tmpLowGamm, ifelse(Group == "Nonusers", Color1, Color2))
LowGammaImgs <- with(tmpLowGamm, ifelse(Group == "Nonusers", Img1, Img2))
LowGammaImgsSz <- with(tmpLowGamm, ifelse(Group == "Nonusers", Img1sz, Img2sz))

tmpHighGamm <- filter(data, Gamma_Band == "High Gamma")
HighGammas = tmpHighGamm$Oscillatory
HighGammasColor <- with(tmpHighGamm, ifelse(Group == "Nonusers", Color1, Color2))
HighGammasImgs <- with(tmpHighGamm, ifelse(Group == "Nonusers", Img1, Img2))
HighGammasImgsSz <- with(tmpHighGamm, ifelse(Group == "Nonusers", Img1sz, Img2sz))

DotSize = 1.5

n <- length(LowGammas) 
d <- data.frame(y = c(LowGammas, HighGammas),
                colr = c(LowGammasColor, HighGammasColor),
                Images = c(LowGammaImgs, HighGammasImgs),
                ImageSizes = c(LowGammaImgsSz, HighGammasImgsSz),
                x = rep(c(1,2), each=n),
                id = factor(rep(1:n,2)))

Color1 = "#20A4F3"
Color2 = "#68D89B"

d$Img = d$x
d$Img[d$Img==1] = 'Controls.png'
d$Img[d$Img==2] = 'cannabis_new.png'

set.seed(321)
d$xj <- jitter(d$x, amount = .09) 
#d$xj_2 <- jitter(d$z, amount = .09)

Pal <- c(Color1, Color2)

par(mar=c(7,7,6,2.1))

p1 <- ggplot(data=d, aes(y=y, color=colr)) +
  
  #Add geom_() objects
  # geom_point(data = d %>% filter(colr==Color1), aes(x=xj), size = DotSize,
  #            alpha = 1, color = Color1) +
  # 
  # geom_point(data = d %>% filter(colr==Color2), aes(x=xj), size = DotSize,
  #            alpha = 1, color = Color2) +
  # 
  geom_line(aes(x=xj, group=id, colour = colr), alpha = 1) +
  
  geom_image(aes(image=Images, x=xj),size = d$ImageSizes)+
  
  geom_half_violin(
    data = d %>% filter(x=="1" & colr==Color1),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = Color1, alpha = .5, color = Color1, trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="1" & colr==Color2),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = Color2, alpha = .5, color = Color2, trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="2" & colr==Color1),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = Color1, alpha = .5, color = Color1, trim = TRUE) +
  geom_half_violin(
    data = d %>% filter(x=="2" & colr==Color2),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = Color2, alpha = .5, color = Color2, trim = TRUE) +

#Define additional settings
  scale_x_continuous(breaks=c(1,2), labels=c("Low Gamma", "High Gamma"), limits=c(0.1, 2.9)) +
  scale_color_manual(values=Pal) + 
  #  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c("Before", "During", "After","Follow", "Before", "During", "After", "Follow"), limits=c(0, 9)) +
  xlab("Frequency Range") + ylab("Power (%)") + ggtitle("Oscillatory") + 
  #ggtitle('Figure 13: Repeated measures with jittered datapoints and connections') +
  theme_classic() +
  theme(legend.position = "none") + 
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(ylim=c(-15, 30))


plot(p1)
ggsave("ViolinPlot_LowHighGamma_Oscillatory.tiff", width = 5, height = 5)

