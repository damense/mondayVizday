# 5Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library("ggimage")


# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 32)
episodes <- tuesdata$episodes
sauces <- tuesdata$sauces
seasons <- tuesdata$seasons

broken <- data.frame(season=c(1,1,2,2,2,
                              3,4,5,7,7,
                              8,10,12,15,17),
                     episode_season=c(1,8,12,13,17,
                                      4,19,1,5,6,
                                      8,10,3,3,10),
                     broke=c(NA,3,7,8,8,
                             8,8,6,8,8,
                             9,8,8,8,8)) 
d <- data.frame(x = 2.5,
                y = 5e5,
                image = "hot_ones.png")

sauces_plus <- sauces %>%
        left_join(episodes %>%
                          select(season, episode_season, guest), multiple = "all") %>%
        left_join(broken,
                  by=c("season","episode_season"))

my_breaks <- c(1000,10000,100000, 1000000)

ggplot(data=sauces_plus %>%
               mutate(guest=ifelse(is.na(broke),
                                   NA,
                                   guest),
                      straw=sauce_number-broke) %>%
               filter(straw==0),
       aes(x=factor(sauce_number),
           y=scoville)) +
        geom_point(data=sauces_plus %>%
                           select(season,sauce_number, scoville) %>%
                           distinct(),
                   aes(x=factor(sauce_number),
                       y=scoville,
                       color=scoville),
                   size=2,
                   position = position_jitter(0.2,
                                              seed = 31))+
        geom_point( aes(color=scoville),
                size=2, 
                   position=position_jitter(0.2,
                                            seed = 31)
                   ) +
        geom_label_repel(aes(label=guest,
                 #           nudge_y=scoville*0.0001
                 ),
                 alpha=0.8,
                         force_pull=0.1,
                         force=1,
                 max.overlaps = 100,
                   position = position_jitter(0.2,
                                              seed = 31),
                   min.segment.length = 0)+
        geom_image(data = d,
                   aes(x=x,y=y,image=image),
                   size=0.3)+
        scale_color_continuous(name="Scoville",
                               type="gradient",
                               low="yellow",
                               high="darkred",
                               trans="log",
                               breaks = my_breaks, labels = my_breaks)+
        scale_y_log10(labels = scales::label_log()) +
    #    scale_color_brewer(palette="Dark2", trans="log")+
        theme_bw() +
        theme(legend.position = "none")+
        xlab("Sauce Number") +
        ylab("Scoville factor")
ggsave("week32.jpg")
