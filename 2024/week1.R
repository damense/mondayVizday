library(tidyverse)
library(sf)
library(ggfx)
library(giscoR)
library(osmdata)
library(httr)

setwd("C:/Users/dmend/OneDrive/Desktop/code/R/mondayVizday/2024")
raw_data <- read.csv("C:/Users/dmend/Downloads/reistransacties2023.csv",
                     sep=";")
stations <- stations <- read.csv("C:/Users/dmend/Downloads/train_stations_europe.csv",
                                 sep=",") %>%
        filter(country =="NL",
               longitude<8) %>%
        mutate(name= ifelse(
                name=="Amsterdam-Centraal",
                "Amsterdam Centraal",
                name),
               name= ifelse(name=="Arnhem",
                            "Arnhem Centraal",
                            name),
               name = ifelse(name == "Den Haag Laan van Nieuw Oost Indie",
                             "Den Haag Laan van NOI",
                             name)
        ) %>%
        select(name, latitude, longitude)
        

SHPs <- list.files(path=".", pattern=".*railway.*", recursive = TRUE)
rails <- lapply(SHPs, function(rail_shp) {
        rail <- sf::st_read(rail_shp) %>%
                st_set_crs(4032)
        return(rail)
}) %>% 
        bind_rows() #finally, merge rail lines into single data.frame



trips_2023 <- raw_data %>%
        filter(Product =="Reizen op Rekening Trein",
               Vertrek != Bestemming) %>%
        left_join(stations, relationship = "many-to-many",
                  by=join_by(Vertrek == name)) %>%
        left_join(stations,relationship = "many-to-many",
                  by=join_by(Bestemming == name),
                  suffix=c(".ver",".bes")) %>%
        select(Datum, Vertrek, Bestemming, 
               latitude.ver, longitude.ver,
               latitude.bes, longitude.bes)

NL_border <- giscoR::gisco_get_countries(country = c("Netherlands"
                                                #     "Belgium",
                                                 #    "Germany",
                                                 #    "Macedonia",
                                                 #    "Serbia",
                                                 #    "Hungary",
                                                 #    "Austria"
                                                ),
                                         epsg=3035,
                                         resolution = "3", 
                                         year=2020)
 ggplot()+
         geom_sf(data=NL_border,
                 fill="lightgrey",
                 color="black",
                 linewidth=1.5)+
         geom_sf(data=rails, 
                 size=3,color="darkblue")+
         geom_sf(data=trips_2023 %>%
                            st_as_sf(coords = c("longitude.ver",
                                                "latitude.ver")) %>%
                            st_set_crs(4032),
                    color="red", size=2)+
         geom_sf(data=trips_2023 %>%
                            st_as_sf(coords = c("longitude.bes",
                                                "latitude.bes")) %>%
                            st_set_crs(4032),
                    color="red", size=2) +
         theme_void()

# ggplot()+
#         geom_sf(data = sf::st_read(
#                 paste0("./train_rails_NL/z_holland/",tp[17])
#                 ) %>%
#                         st_set_crs(4032))
