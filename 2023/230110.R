# 14-Jan-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(sf)
library(ggplot2)
library(raster)
library(mapproj)
library(readxl)
library(jpeg)
library(ggimage)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----
tuesdata <- tidytuesdayR::tt_load(2023, week = 2)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')
name_dictionary <- readxl::read_excel("data/FeederWatch_Data_Dictionary.xlsx", 
                                   "Species Codes")
colnames(name_dictionary) <- name_dictionary[1,]
name_dictionary <- name_dictionary[-1,c(1,3)]
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


canada_map <- getData("GADM", country="CAN", level=1)
us_map <- getData("GADM", country="US", level=1)

# tidying ----
# Taking out Hawaii from the maps
continental_usa <- (us_map[!us_map@data$NAME_1=="Hawaii",])

# Filtering out the rare birds
freq_sights <- as.data.frame(table(feederwatch$species_code)[order(-table(feederwatch$species_code))])
top_birds <- freq_sights[freq_sights$Freq>100,]

top_feederwatch <-feederwatch[feederwatch$species_code %in% top_birds$Var1[1:100],]

# Making a matrix with the seasonal ones (q3-q1<6 months)
top_seasoned_birds <- top_feederwatch %>%
        group_by(species_code) %>%
        summarise(q1=quantile(Month,na.rm=T)[2],
                  q2=quantile(Month,na.rm=T)[3],
                  q3=quantile(Month,na.rm=T)[4]) %>%
        filter(q1+6>q3)
# asigning seasons to the different birds
top_seasoned_birds$season <- ''
top_seasoned_birds$season[top_seasoned_birds$q2>0 & top_seasoned_birds$q2<=3] <- 'Winter'
top_seasoned_birds$season[top_seasoned_birds$q2>3 & top_seasoned_birds$q2<=6] <- 'Spring'
top_seasoned_birds$season[top_seasoned_birds$q2>6 & top_seasoned_birds$q2<=9] <- 'Summer'
top_seasoned_birds$season[top_seasoned_birds$q2>9 & top_seasoned_birds$q2<=12] <- 'Fall'

# plot
plot_data <-right_join(x=top_feederwatch,
                       y=top_seasoned_birds) %>% 
        filter(season == "Spring") %>%
        left_join(y=name_dictionary,
                  by=c("species_code" = "SPECIES_CODE"))

segment_data <- plot_data %>%
        group_by(PRIMARY_COM_NAME) %>%
        summarise(x_start=-exp(mean(log(abs(longitude)))),
                  y_start=exp(mean(log(latitude))))

segment_data$x_end <- c(-120,-70,-140,-150,-60,-160,-140)
segment_data$y_end <- c(65,60,45,65,20,35,20)
segment_data$image <- paste("data/",
                            segment_data$PRIMARY_COM_NAME,
                            ".jpg",
                            sep = "")


ggplot() +
        geom_polygon(data=continental_usa, 
                color="gray",
                aes(long, lat, group=group),
                fill=NA) +
        geom_polygon(data=canada_map,
                aes(long, lat, group=group),
                color="gray", 
                fill=NA) +
        geom_segment(data=segment_data,
                     aes(x=x_start, y=y_start,
                         xend=x_end,yend=y_end,
                         color=PRIMARY_COM_NAME),
                     linewidth=1,
                     show.legend = F)+
        geom_image(data=segment_data, 
                   aes(x=x_end,
                       y=y_end+5,
                       image = image), 
                   size = 0.15)+
        geom_label(data=segment_data,
                   aes(x=x_end,
                       y=y_end+1,
                       label=PRIMARY_COM_NAME),
                   size=3,
                   fill = "white") +
        xlim(c(-180,-50)) +
        ylim(c(20,80)) +
        geom_point(data=plot_data,
                   aes(x=longitude,
                      y=latitude,
                      color=PRIMARY_COM_NAME),
                   show.legend = F) +
        theme_minimal()+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
        )+
        labs(title="Sightings of spring birds", 
             subtitle="Distribution of birds that are seen mostly in the spring") +
        geom_label(aes(x=-120,
                       y=20),
                   size=2,
                   label="Image source: Wikipedia",
                   fill = "white")
ggsave("220110.png")        
