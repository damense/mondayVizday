# 14Aug23
#Author: David Mendez


library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(patchwork)
library(RColorBrewer)
library(jpeg)

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 30)
scurvy <- tuesdata$scurvy %>%
        mutate(gum_rot_d6=factor(substring(gum_rot_d6,3),levels=c("severe","moderate","mild","none")),
               skin_sores_d6=factor(substring(skin_sores_d6,3),levels=c("severe","moderate","mild","none")),
               weakness_of_the_knees_d6=factor(substring(weakness_of_the_knees_d6,3),levels=c("severe","moderate","mild","none")),
               lassitude_d6=factor(substring(lassitude_d6,3),levels=c("severe","moderate","mild","none")),
               fit_for_duty_d6=factor(substring(fit_for_duty_d6,3),levels=c("yes","no"))) 

plot_data <- scurvy %>%
        pivot_longer(cols = c(4:8)) %>%
        mutate(fit=ifelse(name=="fit_for_duty_d6","Fitness","Symptoms"),
               name=str_replace_all(name,"_d6",""),
               name=str_replace_all(name,"_"," "),
               name=str_replace_all(name,"weakness of the knees",
                                    "weakness \nof the knees"),
               treatment=str_replace_all(treatment,"_"," ")) 
p1 <- ggplot(data=plot_data %>%
                     filter(fit=="Fitness"),
             aes(x=name,
                   y=value,
                 group=treatment,
                # fill=treatment,
                   color=treatment))+
        geom_boxplot(size=1) +
        geom_point(position=position_dodge(width=0.75), shape=1, size=3)+
        scale_y_discrete(limits=rev)+
        scale_color_manual(values=brewer.pal(6,"Paired"))+
        #scale_fill_manual(values=brewer.pal(6,"Paired"))+
        theme_bw() +
        xlab("")+
        ylab("") +
        ggtitle("Fit")
p2 <- ggplot(data=plot_data %>%
                     filter(fit!="Fitness"),
             aes(x=name,
                 y=value,
                 group=interaction(treatment,name),
                 #fill=treatment,
                 color=treatment))+
        geom_boxplot(size=1) +
        geom_point(position=position_dodge(width=0.75), shape=1, size=3)+
        scale_y_discrete(limits=rev)+
        scale_color_manual(values=brewer.pal(6,"Paired"))+
        #scale_fill_manual(values=brewer.pal(6,"Paired"))+
        theme_bw()+
        ggtitle("Symptoms")+
        xlab("")+
        ylab("")


layout <- "
#BBB
#BBB
#BBB
#BBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
ABBB
#BBB
#BBB
"
#my_image <- readJPEG("scurvy.jpg", native = T)
combined <- p1+p2 & theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, size=11,face = "bold.italic"),
                          text = element_text(size=14),
                          #axis.text.x = element_text(angle=45,hjust = 1)
                          ) 
combined+
        plot_layout(design = layout,
                    guides = "collect") +
        plot_annotation('Scurvy symptoms on day 6 after treatment', 
                        subtitle = "James Lind, 1757",
                        theme = theme(plot.title = element_text(size = 19),
                                      plot.subtitle = element_text(size=9,
                                                                   hjust = 0.5,
                                                                   face="italic")))
ggsave("week30.png")        
