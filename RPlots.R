

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

# width and height variables for saved plots
w = 6
h = 4
# Define limits of y-axis
y_lim_min = 4
y_lim_max = 7.5


Color1 = "#20A4F3"
Color2 = "#68D89B"


setwd('D:\\Flanker_HIV_Cannabis\\Derivatives\\R')

data <- read_excel("D:\\Flanker_HIV_Cannabis\\Derivatives\\R\\High_Low_Gamma_Spontaneous_Oscillatory.xlsx",
                   sheet = "Sheet1")

###------------Saving the plot

OutFolder = "D:\\Flanker_HIV_Cannabis\\Derivatives\\R\\Figures\\"
OutFileName = "HighLowGammaSpontaneous.tif"

Out <- paste(OutFolder, OutFileName, sep="")


###############################################
########-----------------Spontaneous-------------

tmpLowGamm <- filter(data, Gamma_Band == "Low Gamma")
LowGammas = tmpLowGamm$Spontaneous
HighGammasColor <- with(tmpLowGamm, ifelse(Group == "Nonusers", Color1, Color2))

tmpHighGamm <- filter(data, Gamma_Band == "High Gamma")
HighGammas = tmpHighGamm$Spontaneous
HighGammasColor <- with(tmpHighGamm, ifelse(Group == "Nonusers", Color1, Color2))


DotSize = 5

n <- length(LowGammas) 
d <- data.frame(y = c(LowGammas, HighGammas),
                colr = c(HighGammasColor, HighGammasColor),
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


par(mar=c(7,7,6,2.1))

p1 <- ggplot(data=d, aes(y=y, color=colr)) +
  
  #Add geom_() objects
  geom_point(data = d %>% filter(x=="1"), aes(x=xj), size = DotSize,
             alpha = .6) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x=xj), size = DotSize,
             alpha = .6) +
  
  #Add geom_() objects
  #geom_point(data = d %>% filter(z=="4"), aes(x=xj_2), color = 'green', size = 3,
  #           alpha = .6) +
  #geom_point(data = d %>% filter(z=="5"), aes(x=xj_2), color = 'blue', size = 3,
  #           alpha = .6) +
  #geom_point(data = d %>% filter(z=="6"), aes(x=xj_2), color = 'orange', size = 3, 
  #           alpha = .6) +

  geom_line(aes(x=xj, group=id), color = 'gray', alpha = 1) +
  #geom_line(aes(x=xj_2, group=id), color = 'lightgray', alpha = .3) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = 0), 
    side = "l", fill = '#086375', alpha = .5, color = "#086375", trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = 0), 
    side = "r", fill = "#1dd3b0", alpha = .5, color = "#1dd3b0", trim = TRUE) +
  

  #geom_half_violin(
  #  data = d %>% filter(z=="10"),aes(x = x, y = y), position = position_nudge(x = 7.5), 
  #  side = "r", fill = "green", alpha = .5, color = "#05414D", trim = TRUE) +
  
  #geom_half_violin(
  #  data = d %>% filter(z=="11"),aes(x = x, y = y), position = position_nudge(x = 6.5), 
  #  side = "r", fill = "blue", alpha = .5, color = "#15977D", trim = TRUE) +
  
  #geom_half_violin(
  #  data = d %>% filter(z=="12"),aes(x = x, y = y), position = position_nudge(x = 5.5), 
  #  side = "r", fill = "orange", alpha = .5, color = "#69B203", trim = TRUE) +
  
  #Define additional settings
  scale_x_continuous(breaks=c(1,2), labels=c("Low Gamma", "High Gamma"), limits=c(0.5, 2.5)) +
#  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c("Before", "During", "After","Follow", "Before", "During", "After", "Follow"), limits=c(0, 9)) +
  xlab("Frequency range") + ylab("Power") + ggtitle("Spontaneous") + 
  #ggtitle('Figure 13: Repeated measures with jittered datapoints and connections') +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(ylim=c(0, 25))
  

plot(p1)
ggsave("ViolinPlot_LowHighGamma_Spontaneous.tiff", width = 5, height = 5)


###############################################
########-----------------Oscillatory-------------

tmpLowGamm <- filter(data, Gamma_Band == "Low Gamma")
LowGammas = tmpLowGamm$Oscillatory
LowGammasColor <- with(tmpLowGamm, ifelse(Group == "Nonusers", Color1, Color2))

tmpHighGamm <- filter(data, Gamma_Band == "High Gamma")
HighGammas = tmpHighGamm$Oscillatory
HighGammasColor <- with(tmpHighGamm, ifelse(Group == "Nonusers", Color1, Color2))


DotSize = 5

n <- length(LowGammas) 
d <- data.frame(y = c(LowGammas, HighGammas),
                colr = c(HighGammasColor, HighGammasColor),
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


par(mar=c(7,7,6,2.1))

p1 <- ggplot(data=d, aes(y=y, color=colr)) +
  
  #Add geom_() objects
  geom_point(data = d %>% filter(x=="1"), aes(x=xj), size = DotSize,
             alpha = .6) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x=xj), size = DotSize,
             alpha = .6) +
  
  #Add geom_() objects
  #geom_point(data = d %>% filter(z=="4"), aes(x=xj_2), color = 'green', size = 3,
  #           alpha = .6) +
  #geom_point(data = d %>% filter(z=="5"), aes(x=xj_2), color = 'blue', size = 3,
  #           alpha = .6) +
  #geom_point(data = d %>% filter(z=="6"), aes(x=xj_2), color = 'orange', size = 3, 
  #           alpha = .6) +
  
  geom_line(aes(x=xj, group=id), color = 'gray', alpha = 1) +
  #geom_line(aes(x=xj_2, group=id), color = 'lightgray', alpha = .3) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = 0), 
    side = "l", fill = '#086375', alpha = .5, color = "#086375", trim = TRUE) +
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = 0), 
    side = "r", fill = "#1dd3b0", alpha = .5, color = "#1dd3b0", trim = TRUE) +
  
  
  #geom_half_violin(
  #  data = d %>% filter(z=="10"),aes(x = x, y = y), position = position_nudge(x = 7.5), 
  #  side = "r", fill = "green", alpha = .5, color = "#05414D", trim = TRUE) +
  
  #geom_half_violin(
  #  data = d %>% filter(z=="11"),aes(x = x, y = y), position = position_nudge(x = 6.5), 
  #  side = "r", fill = "blue", alpha = .5, color = "#15977D", trim = TRUE) +
  
  #geom_half_violin(
#  data = d %>% filter(z=="12"),aes(x = x, y = y), position = position_nudge(x = 5.5), 
#  side = "r", fill = "orange", alpha = .5, color = "#69B203", trim = TRUE) +

#Define additional settings
scale_x_continuous(breaks=c(1,2), labels=c("Low Gamma", "High Gamma"), limits=c(0.5, 2.5)) +
  #  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c("Before", "During", "After","Follow", "Before", "During", "After", "Follow"), limits=c(0, 9)) +
  xlab("Frequency range") + ylab("Power") + ggtitle("Oscillatory") + 
  #ggtitle('Figure 13: Repeated measures with jittered datapoints and connections') +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(ylim=c(-20, 40))


plot(p1)
ggsave("ViolinPlot_LowHighGamma_Oscillatory.tiff", width = 5, height = 5)

