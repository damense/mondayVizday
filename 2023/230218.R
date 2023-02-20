# 18-Feb-23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library(RColorBrewer)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 07)
dataset <- tuesdata$age_gaps

data_ord <- dataset[order(-dataset$release_year),]

same_df <- data_ord[0,]
# same_df$label <- ''
for (i in 1:dim(data_ord)[1]){
        year <- data_ord$release_year[i]
        actor <- data_ord$actor_1_name[i]
        age_2 <- data_ord$actor_2_age[i]
        data_chunk <- data_ord %>%
                filter(actor_1_name==actor,
                       release_year <= year-age_2,
                       )
        if(dim(data_chunk)[1]>0){
                new_df <- rbind(data_ord[i,],
                                data_chunk[1,])
                # new_df$label <- c(paste(data_ord$actor_2_name[i],
                #                       "was not born\n when",
                #                       data_ord$actor_1_name[i],
                #                       "(their couple in",
                #                       data_ord$movie_name[i],
                #                       ")\n was filming",
                #                       data_chunk$movie_name[1],
                #                       "alongside \n",
                #                       data_chunk$actor_2_name[1],
                #                       "(then",
                #                       data_chunk$actor_2_age[1],
                #                       ") "),
                #                   '')
                same_df <- rbind(same_df,
                                 new_df)
        }
}
text_df <- data.frame(actor=character(0),
                      x0=numeric(0),
                      x1=numeric(0),
                      y0=numeric(0),
                      y1=numeric(0),
                      label=character(0),
                      position=numeric(0)
                      )
k <- 1
while (k<dim(same_df)[1]){
        text_df <- rbind(text_df,
                         data.frame(actor=same_df$actor_1_name[k],
                                 x0=same_df$release_year[k],
                                    x1=same_df$release_year[k+1],
                                    y0=same_df$age_difference[k],
                                    y1=same_df$age_difference[k+1],
                                    label=paste(same_df$actor_2_name[k],
                                                "was not born when",
                                                same_df$actor_1_name[k],
                                                "\n(their partner in",
                                                same_df$movie_name[k],
                                                ")\n was filming",
                                                same_df$movie_name[k+1],
                                                "alongside \n",
                                                same_df$actor_2_name[k+1],
                                                                      "(then",
                                                                      same_df$actor_2_age[k+1],
                                                                      ") "),
                                 position=k/dim(same_df)[1]
                         )
                         )
        
        
        k <- k+2
}
same_df$actor_1_name <- factor(same_df$actor_1_name)

ggplot()+
        geom_point(data=dataset,
                   aes(x=release_year,
                       y=age_difference),
                   color="darkgrey")+
        geom_point(data=same_df,
                   aes(x=release_year,
                       y=age_difference,
                       color=actor_1_name)
                   ) +
        geom_segment(data= text_df,
                     aes(x = x0, y = y0, 
                         xend = x1, yend = y1,
                         color=actor))+
        geom_label_repel(data=text_df,
                aes(x = x0*(1-position)+x1*(position), 
                    y = y0*(1-position)+y1*(position),
                            label=label,
                            color=actor,
                    nudge_x = 100*position
                    ),
                force= 100,
                nudge_y = 2,
                size=2.5
        )+
        theme_bw() +
        scale_color_brewer(palette = "Dark2")+
        labs(title="Age gap between actors in movies", 
             subtitle="with emphasis on big age gaps",
             colour = "Actor name") +
        xlab("Release Year")+
        ylab("Age gap")

