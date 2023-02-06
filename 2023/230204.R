# 4-Feb-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(sf)
library(maps)
library(osmdata)
library(tmap)
library(tmaptools)
library(gganimate)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 05)
cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference


coast <- opq (getbb("Falmouth, UK")) %>%
        add_osm_feature(key="natural",
                        value=c(
                                "coastline", "bay","beach"
                                , "water"
                                )) %>%
        osmdata_sf()
streets <- getbb("Falmouth, UK")%>%
        opq()%>%
        add_osm_feature(key = "highway",
                       value = c("motorway", "primary", "motorway_link", "primary_link",
                                "residential", "living_street",
                               "unclassified",
                              "service", "footway",
                             "secondary", "tertiary", "secondary_link", "tertiary_link")) %>%

        osmdata_sf()
long_min <- -5.086
long_max <- -5.076
lat_min <- 50.160
lat_max <- 50.165
xlabs = seq(long_min,long_max, 0.002)
ylabs = seq(lat_min,lat_max,0.001)
plot_data <- cats_uk %>%
       filter(tag_id=="Magic-Tag"|tag_id=="Smudge-Tag"|tag_id=="Tigger-Tag") 
plot_data <- left_join(plot_data,
                       cats_uk_reference,
                       by=c("tag_id"))
 


p <- ggplot(data=plot_data,
       aes(x=location_long,
           y=location_lat,
           color=animal_sex,
           shape=animal_id))  +
        geom_sf(data = coast$osm_multipolygons,
                inherit.aes = FALSE,
                fill = "lightblue") +
        geom_sf(data = coast$osm_polygons,
                inherit.aes = FALSE,
                fill = "lightblue")+
        geom_sf(data = coast$osm_lines,
                inherit.aes = FALSE,
                fill = "black") +
        geom_sf(data = streets$osm_lines,
                inherit.aes = FALSE,
                fill = "black")+
        geom_point(show.legend = T, size=4 ,stroke = 2) + 
        scale_shape(solid = FALSE, name ="Cat's name")+
        scale_color_discrete(name ="Cat's sex")+
        geom_label(x=-5.081, y =50.163,inherit.aes = FALSE,
                   label="Falmouth",
                   size=5,
                   fill = "white") +
        scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
        scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) +
        coord_sf(   xlim = c(long_min,long_max), 
                    ylim = c(lat_min, lat_max)) +
        theme_bw() +
        theme(
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title = element_text(size=18)
        )+
        labs(title="Magic, Smudge and Tigger", 
             subtitle="A love story in Northern Falmouth, UK")+
        transition_reveal(timestamp)

anim_save("230204.gif", p)
