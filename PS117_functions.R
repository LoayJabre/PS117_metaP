#Plots

##########
#for plots
Fetreatment_order <- c('T0', 'noFe', 'Fe')
temptreatment_order <- c('T0', 'LT', 'HT')
treatment_order <- c('T0_T0', 'LT_noFe', 'LT_Fe', 'HT_noFe', 'HT_Fe')
#######
#this plots a variable against time (e.g. Fe vs time)
x_vs_time <- function (data, xaxis, yaxis, yaxislabel) {
  ggplot(data, aes(x = {{xaxis}}, 
                   y= {{yaxis}},
                   color = temperature_treatment, 
                   shape = iron_treatment)) + 
    stat_summary(fun = mean, geom = "point", show.legend = F, size = 4, stroke = 1.5, alpha = 0.5)+
    stat_summary(fun = mean, geom = "line", size = 0.9, aes(linetype = iron_treatment))+
    stat_summary(fun.data =  mean_se, geom = "errorbar", size = 1, width = 0.1) +
    
    ylab (substitute(yaxislabel)) + 
    xlab (expression ("Day Count")) +
    
    scale_color_manual(name="Temperature",
                       breaks = c("LT","HT"),
                       labels = c("LT", "HT"),
                       values = c('black', "darkorange", "blue")) +
    
    scale_linetype_manual(name="Iron",
                          breaks = c("Fe","noFe"),   #this removes the T0 from the legend
                          labels = c("Fe", "noFe"),
                          values = c("solid", "dotted", "solid")) +
    
    scale_shape_manual(name = "Iron",
                       breaks = c("noFe","Fe", "T0"), 
                       labels = c("noFe", "Fe", "T0"),
                       values = c(1, 19, 5))+
    theme_bw()+
    theme(axis.title.x=element_text(size = 14,face = "bold", color = "black"), 
          axis.text.x=element_text(face = "bold", size = 12, color = "black", vjust = 0.5, hjust = 1),
          axis.title.y=element_text(size=18, color = "black"), 
          axis.text.y=element_text(face = "bold", size = 12, color = "black"),
          strip.background =element_rect(fill = "white"),
          strip.text.x = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 14)) +   #this keeps the T0 formatting that I want
    
          facet_wrap(~bioassay)
  
}




##################################
#this plots a variable vs treatment (e.g. Fe vs temperature)
x_vs_treatment <- function (data, xaxis, xaxislevel, yaxis,yaxislabel) {
  
  ggplot(data, aes(
    x = (factor({{xaxis}}, level = xaxislevel)),
    y = {{yaxis}},
    color = temperature_treatment,
    shape = iron_treatment)) + 
    facet_wrap(~bioassay)+
    #geom_point(size = 5, alpha=0.5, stroke = 1.8)+
    stat_summary(fun = mean, geom = "point", show.legend = T, size = 5, stroke = 1.8, alpha = 0.5)+
    stat_summary(fun.data =  mean_se, geom = "errorbar", show.legend = F, size = 1.3, width = 0.1)+
    
    scale_color_manual(name="Temperature",
                       breaks = c("LT","HT"),
                       labels = c("ambient", "warm"),
                       values = c('darkorange2', 'black', 'black')) +
    scale_shape_manual(name = "Iron",
                       breaks = c("Fe","noFe"), #this removes the T0 from the legend
                       labels = c("Fe", "noFe"),
                       values = c(19, 1, 1)) + 
    
    ylab (substitute(yaxislabel)) + 
    xlab (expression ("")) +
    
    theme_bw()+
    theme(axis.title.x=element_text(size = 20,face = "bold", color = "black"), 
          axis.text.x=element_text(face = "bold", size = 15, color = "black", vjust = 0.5, hjust = 0.5),
          axis.title.y=element_text(size=20, color = "black"), 
          axis.text.y=element_text(face = "bold", size = 15, color = "black"),
          strip.background =element_rect(fill = "white"),
          strip.text.x = element_text(size = 15, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 14))
}

#####