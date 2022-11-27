# 28-Nov-22
#Author: David Mendez

library(tidyr)
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(spData)
library(purrr)




# Read the dataset ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 47)[[1]]


# create the map
tuesdata_london <- tuesdata [tuesdata$Longitude > -0.6 &
                                      tuesdata$Longitude < 0.4 & 
                                      tuesdata$Latitude > 51.25 & 
                                      tuesdata$Latitude < 51.71,
                             ]
tuesdata_london <- tuesdata_london %>%
        mutate_all(str_replace_all, "  ", " ")
tuesdata_london$topic <- unlist(map(str_split(tuesdata_london$Subject_Matter, "-"), 1))

ggplot(data=lnd) + geom_sf()+
        theme_void()  +
        geom_point(data=tuesdata_london,
                   aes(x = as.numeric(Longitude),
                       y = as.numeric(Latitude),
                       color = topic),
                   size = 2.5,
                   alpha = .6)  +
        facet_wrap(~ topic, ncol = 7) +
        coord_sf(xlim=c(-0.6,0.4),ylim=c(51.25,51.71)) + 
        scale_color_discrete(name = "Topics") +
        labs(title="Topics of museums \nof London\n") +
        theme(plot.title = element_text(color="black", size=21, hjust = 0.5),
              legend.position = "none")


