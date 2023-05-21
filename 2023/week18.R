# 01 - May-23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 18)

surveys <- tuesdata$surveys
species <- tuesdata$species

plot_data <- surveys %>%
        left_join(species, by=c("species")) %>%
        mutate(rangehfl=maxhfl-minhfl,
               rangewgt=maxwgt-minwgt) 

p1 <- ggplot(data=plot_data) +
        geom_boxplot(aes(x=commonname,
                         y=wgt
#                         y=hfl
                         )) +
        geom_line(aes(x=commonname,
                      y=minwgt,
#                      y=minhfl,
                      group=1),
                  linetype=2) +
        geom_line(aes(x=commonname,
                      y=maxwgt,
#                      y=maxhfl,
                      group=1),
                  linetype=2) +
        theme_bw() +
        xlab("") +
        #ylab("Hindfoot Length")+
        ylab("Weight")+
        theme(axis.text.x = element_blank())

p2 <- ggplot(data=plot_data) +
        geom_boxplot(aes(x=commonname,
                         #y=wgt
                         y=hfl
        )) +
        geom_line(aes(x=commonname,
                      #y=minwgt,
                      y=minhfl,
                      group=1),
                  linetype=2) +
        geom_line(aes(x=commonname,
                      #y=maxwgt,
                      y=maxhfl,
                      group=1),
                  linetype=2) +
        theme_bw() +
        xlab("") +
        ylab("Hindfoot Length")+
        #ylab("Weight")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

figure <- ggarrange(p1,p2,nrow = 2, heights = c(0.55,1))

annotate_figure(figure, top = text_grob("Length and Weight distributions of desert rodents \nin Arizona, US \n", 
                                      color = "black", face = "bold", size = 14, family = "Constantia"))
ggsave("week18.jpg")
