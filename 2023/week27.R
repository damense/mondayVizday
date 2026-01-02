# 5Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(extrafont)
library(sf)
library(maps)
library(osmdata)
library(tmap)
library(tmaptools)
library(gganimate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 27)
historical_markers <- tuesdata$historical_markers
no_markers <- tuesdata$no_markers

        

# r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
# nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
# nyc_neighborhoods_sf <- st_as_sf(nyc_neighborhoods)
streets <- getbb("Fort Oglethorpe, USA")%>%
        opq()%>%
        add_osm_feature(key = "highway",
                        value = c("motorway", "primary", "motorway_link", "primary_link",
                                  "residential", "living_street",
                                  "unclassified",
                                  "service", "footway",
                                  "secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
                osmdata_sf()
borders <- getbb("Fort Oglethorpe, USA")%>%
        opq()%>%
        add_osm_feature(key = "boundary",
                value = "administrative") %>%
        osmdata_sf()
        
        
        

historical_markers$Decade <- "Unknown"
historical_markers$Decade[historical_markers$year_erected>1959 &
                           historical_markers$year_erected<1970] <- "1960s"
historical_markers$Decade[historical_markers$year_erected>1969 &
                                  historical_markers$year_erected<1980] <- "1970s"
historical_markers$Decade[historical_markers$year_erected>1979 &
                                  historical_markers$year_erected<1990] <- "1980s"
historical_markers$Decade[historical_markers$year_erected>1989 &
                                  historical_markers$year_erected<2000] <- "1990s"
historical_markers$Decade[historical_markers$year_erected>1999 &
                                  historical_markers$year_erected<2010] <- "2000s"
historical_markers$Decade[historical_markers$year_erected>2009 &
                                  historical_markers$year_erected<2020] <- "2010s"
historical_markers$Decade[historical_markers$year_erected>2019 &
                                  historical_markers$year_erected<2030] <- "2020s"


ggplot()+
        geom_sf(data = streets$osm_lines,
                inherit.aes = FALSE,
                 fill = "black")+
        geom_sf(data = borders$osm_lines %>%
                        filter(admin_level==8),
                inherit.aes = FALSE,
                fill = "black")+
        theme_bw()+
        geom_point(data=historical_markers %>%
                           filter(city_or_town =="Fort Oglethorpe",
                                  !is.na(year_erected)),
                   aes(x= longitude_minus_w,
                       y=latitude_minus_s,
                       color=year_erected),
                   size=3) +
        theme(axis.text=element_blank(),
              axis.title = element_blank())+
        ggtitle("Historical Markers in Fort Oglethorpe")
