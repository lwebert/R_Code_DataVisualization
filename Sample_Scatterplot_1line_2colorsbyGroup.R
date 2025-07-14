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


    
 ######################Low Gamma with RT#######################


    p1 <- ggplot(data, aes(x = Avg_Combined, y = LowGamma_LH_Postcentral_Abs_Spont, colour = Group)) +
      geom_point(size = dot_size) +
      
      #####Change colors here#########
      scale_color_manual(values = c("#9F007A", "#70A9A1")) +
      geom_smooth(method=lm , color=best_fit_line_color, fill=best_fit_SEM_color, se=TRUE, size = 2) +
      
      xlab("Reaction Time (ms)") +
      theme(axis.title.x = element_text(size = axis_title_text_size)) +
      theme(axis.text.x = element_text(size = axis_text_text_size)) +
      
      ylab(expression ("Spontaneous Power "~(nAm^2))) + 
      theme(axis.title.y = element_text(size = axis_title_text_size)) +
      theme(axis.text.y= element_text(size = axis_text_text_size))+
      
      #########I included the legend so you know which group is which color for now
      #theme(legend.position="none") +
      theme(legend.position="right") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
   
    plot(p1)
    
    ggsave("Scatterplot_Example_ColorsbyGroup.tiff", width = 5, height = 5)
    
    