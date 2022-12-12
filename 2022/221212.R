# 12-Dec-22
#Author: David Mendez

library(tidyr)
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(spData)
library(purrr)
library(tigris)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(viridis)
library(ggthemes)




# Read the dataset ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 49)[[1]]
nyc_sales <- read.csv('data/nyc-rolling-sales.csv')
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


# merge the census datasets


nyc_raw_data <- merge(tuesdata,
                      nyc_sales,
                      by.x=c("TAX_BLOCK", "TAX_LOT"),
                      by.y = c("BLOCK", "LOT"),
                      all=T)
nyc_raw_data$SALE.PRICE <- as.numeric(nyc_raw_data$SALE.PRICE)
nyc_raw_data$DV_FLOOR_TO <- as.numeric(nyc_raw_data$DV_FLOOR_TO)
nyc_neighborhoods_sf <- st_as_sf(nyc_neighborhoods)
manh_map <- filter(nyc_neighborhoods_sf,borough == "Manhattan")
ggplot()+
        geom_point(data=nyc_raw_data[nyc_raw_data$Borough == "Manhattan" & nyc_raw_data$`Device Type` == "Passenger Elevator (P)",
                                     ],
                   aes(x=LONGITUDE, 
                       y=LATITUDE, 
                       color = log10(DV_FLOOR_TO)
                       ),
                   size=0.5) + 
        geom_sf(data=manh_map, 
                     color="gray", 
                     fill=NA)+
        #facet_wrap(~`Device Type`, nrow = 2)+
        scale_color_viridis_b(name="Maximun \nfloor", 
                              labels=c("<10",
                                       ">10 & <100",
                                       ">100 & <1000"
                                       ))+
        theme_classic()+
        theme(legend.position = "right",
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              )+
        labs(title="Maximum floor reached", subtitle=" by passenger elevators in Manhattan")
        
ggsave("221211.png")
