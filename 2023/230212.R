# 12-Feb-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggimage)






setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2019, week = 11)

# wrangle ----
subset <- tuesdata$board_games %>%
        select(name, artist, designer, year_published, average_rating, users_rated) %>%
        filter(users_rated>10000) %>%
        separate_rows(artist, sep = ",", convert = TRUE) %>%
        separate_rows(designer, sep = ",", convert = TRUE)

most_popular_artist <- subset %>%
        group_by(artist) %>%
        top_n(1, average_rating) %>%
        arrange(desc(average_rating)) %>%
        select(artist) %>%
        distinct %>%
        head(10) %>%
        unlist(use.names = F)






#most_popular_designer <- table(subset$designer)[order(-table(subset$designer))][1:10]
plot_data <- tuesdata$board_games %>%
        separate_rows(artist, sep = ",", convert = TRUE) %>%
        filter(artist %in% most_popular_artist ) 

summarized_data <- plot_data %>%
        group_by(artist) %>%
        summarise(n=n(), mean=mean(average_rating))

top_bg <- plot_data %>%
        group_by(artist) %>%
        top_n(1, average_rating) %>%
        select(year_published, name,artist, average_rating)


# plot ----
ggplot() +
        geom_point(data=plot_data,
                   aes(x=year_published,
                       y=average_rating,
                       color=category
                       )) +
        geom_hline(data=summarized_data,
                   aes(yintercept=mean),
                   linetype=2)+
        geom_text(data=top_bg,
                  x=2002,
                   aes(
                       y=average_rating+0.5,
                       label=name),
                  size=3,
                  show.legend = F)+
        facet_wrap(~artist, nrow = 4) +
        theme_bw()
        