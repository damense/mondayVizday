library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2023, week = 51)

holiday_episodes <- tuesdata$holiday_episodes
holiday_episode_genres <- tuesdata$holiday_episode_genres

holiday_episodes %>% 
        pivot_longer(cols = christmas:holiday, 
                     names_to = "type", 
                     values_to = "values") %>% 
        filter(values == TRUE, year>2000)  %>% 
        ggplot(aes(x=year)) + 
        geom_bar(aes(fill=type))+
        theme_bw()

ggplot(data=holiday_episodes, aes(x=year, y = average_rating)) +
        geom_point(color="grey")+
        geom_point(data=holiday_episodes %>%
                           filter(parent))
        theme_bw()
