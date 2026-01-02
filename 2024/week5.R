library(tidyverse)
library(tidytuesdayR)
library(GGally)
library(raster)
library(gganimate)
library(magick)


# get data ----
tuesdata <- tidytuesdayR::tt_load(2024, week = 5)
predictions <- tuesdata$predictions
groundhog <- tuesdata$groundhogs
usa <- map_data("usa")
canada <- map_data("world", "Canada")
mex <- map_data("world", "Mexico")

all_data <- predictions |> 
        left_join(
                groundhog,
                join_by(id)
        ) %>%
        filter(!is.na(shadow)) %>%
        dplyr::select(-details, -source,
               -current_prediction,
               -description)
# check data ----
summary(all_data)
head(all_data)
glimpse(all_data)
p <- ggplot()+
        geom_polygon(data=usa,
                     color="black",
                     aes(long, lat, group=group),
                     fill="lightgray")+
        geom_polygon(data=canada,
                     color="black",
                     aes(long, lat, group=group),
                     fill="lightgrey")+
        geom_polygon(data=mex,
                     color="black",
                     aes(long, lat, group=group),
                     fill="lightgrey")+
        geom_point(data=all_data,
                   aes(x=longitude,
                       y=latitude),
                   color="black",
                   size=3)+
        geom_point(data=all_data,
                   aes(x=longitude,
                       y=latitude,
                       color=shadow),
                   size=2)+
        theme_minimal()+
        coord_fixed(xlim = c(-130, -60),  ylim = c(25, 55), ratio = 1.5)+
        scale_colour_manual(values = c("white", "black"))+
        transition_time(time = year,
                        )+
        labs(title = 'Did the Groundhog see his shadow?',
             subtitle='Year {as.integer(frame_time)}',
             caption = "Black is yes, white is no\n @dmendsev@fosstodon.org") +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = "none",
              title = element_text(size=14, family = "mono"))
animate(p, width=2.5*240,height=2.5*150,fps=2)
gganimate::anim_save("week5.gif")
