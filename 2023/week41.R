library(tidytuesdayR)
library(tidyverse)
library(maps)
library(mapdata)

tuesdata <- tidytuesdayR::tt_load(2023, week = 41)

haunted_places <- tuesdata$haunted_places


washington <- subset(state, region=="washington")
counties <- map_data("county")
washington_county <- subset(counties, region=="washington")

ggplot(data=washington, mapping=aes(x=long, y=lat, group=group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color="black", fill="gray") + 
        geom_polygon(data=washington_county, fill=NA, color="white") + 
        geom_polygon(color="black", fill=NA) + 
        ggtitle('Washington Map with Counties') + 
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
        geom_point(data=haunted_places %>%
                           filter(state_abbrev %in% c("WA"),
                                  longitude< -120),
                   aes(x=longitude,
                       y=latitude),
                   inherit.aes = F) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())  
