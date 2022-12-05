# 05-Dec-22
#Author: David Mendez

library(tidyr)
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(spData)
library(purrr)
library(lubridate)
library(ggthemes)


# Read the dataset ----
raw_data <- read.csv('data/movieData.csv')

genre_deff <- c("Action", "Adventure", "Animation", "Comedy", "Crime",
                   "Documentary","Drama", "Family", "Fantasy", "History", 
                   "Horror", "Music", "Mystery", "Romance", "Science Fiction", 
                   "TV Movie", "Thriller", "War", "Western") 
genre_id <- c(28, 12, 16, 35, 80,
              99, 18, 10751, 14, 36,
              27, 10402, 9648, 10749, 878,
              10770, 53, 10752, 37)
pallette <- c(
        "dodgerblue2", "#E31A1C", # red
        "green4",
        "#6A3D9A", # purple
        "#FF7F00", # orange
        "black", "gold1",
        "skyblue2", "#FB9A99", # lt pink
        "palegreen2",
        "#CAB2D6", # lt purple
        "#FDBF6F", # lt orange
        "gray70", "khaki2",
        "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
        "darkturquoise", "green1", "yellow4", "yellow3",
        "darkorange4", "brown"
)

#clean de dataset----
clean_data <- raw_data
clean_data$adult[raw_data$adult=='False'] <- FALSE
clean_data$video[raw_data$video=='False'] <- FALSE
clean_data$release_date <- as.Date(raw_data$release_date,format = "%Y-%m-%d")

for (i in 1:length(genre_id)){
        clean_data[,genre_deff[i]] <- FALSE
        clean_data[grep(genre_id[i], raw_data$genre_id),genre_deff[i]] <- TRUE
}

#plot
plot_data <-  clean_data %>%
        group_by(yr = year(release_date)) %>% 
        summarise(mn_vote = mean(vote_average), 
                  mx_vote=max(vote_average),
                  cnt_vote = sum(vote_count),
                  n_movies = n()) 
mdf <- melt(df,id.vars="yr")
 ggplot() +
#        geom_point(data=plot_data,
 #                 aes(x=yr, y=mn_vote))+
         geom_point(data=plot_data,
                    aes(x=yr, y=mn_vote), color=pallette[1]) +
         geom_smooth(data=plot_data,
                     aes(x=yr, y=mn_vote),
                     method = "lm")+
         geom_point(data=plot_data,
                    aes(x=yr, y=log10(n_movies)*2.5), color=pallette[2]) +
         geom_smooth(data=plot_data[plot_data$yr>1958,],
                     aes(x=yr, y=log10(n_movies)*2.5),
                     color=pallette[2],
                     method = "loess")+
         
         scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
         scale_y_continuous( name = "Yearly Mean Vote\n",
                             limits = c(0,10),
                 sec.axis = sec_axis(~.*1/2.5,
                                     labels=function(x) round(10^x),
                                     breaks=c(0:5, 0:5+1),
                                     name="Number of movies")
         ) +
         ggtitle("Queer movies through the years (TMDB)",
                 subtitle = '\nAmount of movies and mean of votes\n') +
         xlab("\nYear")+
         theme_economist() + scale_colour_hc()

 # for (i in 1:length(genre_id)){
#         p <- p+geom_histogram(data=clean_data[clean_data[,genre_deff[i]]==TRUE,],
#                               aes(x=release_date),
#                               alpha=0.5, position="identity", 
#                               binwidth=30
#         )
# }
#        p+scale_color_manual(values=pallette[1:19])+
#                scale_fill_manual(values=pallette[1:19])
