# 27-Apr-23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(gganimate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 17)
winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon
london_weather <- read.csv("data/london_weather.csv")
winners$distance <- 42.195
anima_data <- winners %>%
        mutate(distance=0,
               Time=Time-Time)
for (i in 1:42){
        new_df <- winners %>%
                mutate(distance=i,
                       Time=Time/42.195*i)
        anima_data <- rbind(anima_data, new_df)
}
anima_data <- rbind(anima_data, winners)
ggplot(anima_data %>%
               filter(Category=="Women") %>%
               arrange(Time, Year)) +
        geom_line(aes(x=distance,
                       y=Athlete,
                       color=Year,
                   #    group=seq_along(Athlete))
                   ))+
        theme_bw() +
        transition_reveal(Time)+
        ease_aes('linear')
