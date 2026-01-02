# 5Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 22)
centenarians <- tuesdata$centenarians
centenarians$initial <- factor(substr(centenarians$name,1,1))

world_coordinates <- map_data("world") %>%
        filter(region == "Japan") %>%
#        mutate(region = ifelse(region == "USA", "United States", region),
#               region = ifelse(region == "Antarctica","", region))%>%
        left_join(data.frame(table(centenarians$place_of_death_or_residence)),
                  by=join_by(region==Var1))
p1 <- ggplot(world_coordinates, 
       aes(x = long, y = lat, group = group)) +
        geom_polygon(fill="Blue") +
        theme_void() +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())


p2 <- ggplot(data=centenarians %>%
               filter(place_of_death_or_residence=="Japan")) +
        geom_histogram(aes(x=age, fill=gender),
                       bins=30) +
        theme_bw() +
        xlab("Age (years)")+
        ylab("Amount")+
        xlim(c(110,120))+
        ylim(c(0,8))+
        theme(legend.position = 'bottom')

 ggarrange(p1,p2,
          NULL,NULL,
          nrow = 2, ncol=2,
          widths = c(1,1.2),
          heights = c(1.2,1))
