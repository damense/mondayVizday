
# Load packages -----
library(tidycensus)
library(sf) 
library(janitor) 
library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(grid)
library(extrafont)

# functions ----
font_import()
loadfonts(device = "win")
# constants ----

# get data ----

water_insecurity_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
water_insecurity_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')

logo <- png::readPNG("C:/Users/dmend/OneDrive/Desktop/logo.png")

to <- tidycensus::county_laea

# process data ----
process_data <- to |> 
        left_join(rbind(water_insecurity_2022, water_insecurity_2023) |> 
                          dplyr::select(-geometry),
                  by=join_by(GEOID==geoid)) |> 
        
        mutate(year=factor(year),
               state_code=substr(GEOID,1,2)) |> 
        left_join(codes,
                  by=join_by(state_code)) |> 
        select(-total_pop, -plumbing) |> 
        pivot_wider(names_from = year, values_from = percent_lacking_plumbing) |> 
        mutate(change=`2023`-`2022`)

brks2 <- sort(unique(process_data$change))
# plot data----
ggplot(data = process_data |>
               filter(
                       #percent_lacking_plumbing>0,
                       year==2022,
                      state_name=="Washington"),
       aes(y=total_pop, x=percent_lacking_plumbing, group = geoid))+
        geom_point()+
        scale_y_log10()+
        #scale_x_log10()+
        facet_wrap(~state_name)+
        theme_bw()
size <- 0.3e6
ggplot()+
        geom_sf(data=to, aes(geometry=geometry),fill="white",col="grey")+
        geom_sf(data = process_data |> 
                        mutate(region=NA,
                               region = ifelse(state_name %in% c("California","Oregon","Washington"),
                                               "West Coast",region),
                               region = ifelse(state_name %in% c("Connecticut", "Maine", "Massachusetts", 
                                                                 "New Hampshire", "Rhode Island", "Vermont",
                                                                 "New Jersey", "New York", "Pennsylvania"),
                                               "Northeast",region)
                        ) |> 
                        filter(region=="West Coast"),
                 aes(geometry=geometry, fill=change))+
        theme_bw()+
        scale_fill_gradient2()+
        ggtitle("Change in access to plumbing facilities",
                subtitle = "Improvement between 2022 and 2023 in the West Coast")+
        annotation_custom(
                rasterGrob(logo),
                xmin = -2.5e6, xmax = -2.5e6+size,
                ymin = -1.3e6, ymax = -1.3e6+size
        )+
        xlim(c(-2.5e6,-0.7e6))+
        ylim(c(-1.3e6,0.7e6))+
        labs(fill = "Improvement\n[%]\n") + 
        theme(
                text = element_text(family = "Verdana"),
                panel.background = element_rect(fill = "grey95")
        )
