# 21-Jan-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(arthistory)
library(ggimage)
library(splines)
#library(ggformula)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 03)

worksgardner_custom <- worksgardner
worksjanson_custom <- worksjanson

worksgardner_custom$origin <- "worksgardner"
worksjanson_custom$origin <- "worksjanson"
works <- full_join(worksgardner_custom,
                   worksjanson_custom)

spanish_and_dutch_works <- works[works$artist_nationality %in% c("Spanish","Dutch"),]

# fixing the dataset----
#Unifying The Sleep of Reason Produces Monsters
spanish_and_dutch_works$title_of_work[grepl("of Reason",
                                            spanish_and_dutch_works$title_of_work)] <- "The Sleep of Reason Produces Monsters"
#Unifying The Family of Charles IV
spanish_and_dutch_works$title_of_work[grepl("Family of Charles IV",
                                            spanish_and_dutch_works$title_of_work)] <- "The Family of Charles IV"

#Unifying Composition with Red, Blue and Yellow
spanish_and_dutch_works$title_of_work[grepl("Red, Blue",
                                            spanish_and_dutch_works$title_of_work)] <- "Composition with Red, Blue and Yellow"
#unifying Still-life with Chair-caning
spanish_and_dutch_works$title_of_work[grepl("with Chair",
                                            spanish_and_dutch_works$title_of_work)] <- "Still Life with Caned Chair"

#unifying Saturn Devouring One of His Children
spanish_and_dutch_works$title_of_work[grepl("Saturn",
                                            spanish_and_dutch_works$title_of_work)] <- "Saturn Devouring One of His Sons"

#unifying The Starry Night
spanish_and_dutch_works$title_of_work[grepl("Starry Night",
                                            spanish_and_dutch_works$title_of_work)] <- "The Starry Night"

#unifying The Night Cafe
spanish_and_dutch_works$title_of_work[grepl("Night Caf",
                                            spanish_and_dutch_works$title_of_work)] <- "The Night Cafe"

#unifying La berceuse
spanish_and_dutch_works$title_of_work[grepl("Woman Rocking",
                                            spanish_and_dutch_works$title_of_work)] <- "La Berceuse (Woman Rocking a Cradle)"
#unifying Vive la France
spanish_and_dutch_works$title_of_work[grepl("Vive la...",
                                            spanish_and_dutch_works$title_of_work)] <- "Still Life. Vive la France"

#unifying Les Demoiselles d'Avignon
spanish_and_dutch_works$title_of_work[grepl("Avignon",
                                            spanish_and_dutch_works$title_of_work)] <- "Les Demoiselles d'Avignon"
#unifying The third of May
spanish_and_dutch_works$title_of_work[grepl("Third of May",
                                            spanish_and_dutch_works$title_of_work)] <- "The Third of May, 1808"

# plot --------
plot_data <- spanish_and_dutch_works %>% filter(origin=="worksgardner")

max_works <- plot_data %>%
        select(artist_name, title_of_work ,edition_number, space_ratio_per_page, publication_year)%>%
        group_by(edition_number_year) %>%
        slice(which.max(space_ratio_per_page))
max_works$image <- paste("data/",
                            max_works$title_of_work,
                            ".jpg",
                            sep = "")
max_works$label <- max_works$title_of_work
max_works$position <- max_works$space_ratio_per_page+0.5
max_works$position[max_works$title_of_work=="Landscape with Cypress Trees"] <- 2 

for (i in length(max_works$image):2){
        if(max_works$image[i]==max_works$image[i-1]){
                max_works$image[i] <- NA
                max_works$label[i] <- NA
        }
}
        
#        filter(space_ratio_per_page == max(space_ratio_per_page))

ggplot()+
        geom_point(data=plot_data,
                   aes(x=publication_year,
                       y=space_ratio_per_page,
                       group=artist_nationality,
                       fill=artist_nationality),
                   pch=21) +
        geom_smooth(data=plot_data,
                   aes(x=publication_year,
                       y=space_ratio_per_page, 
                       group=artist_nationality,
                       fill=artist_nationality),
                   color="grey",
                   linetype=2)+
        geom_line(data=plot_data[plot_data$title_of_work %in% max_works$title_of_work,],
                  aes(x=publication_year,
                      y=space_ratio_per_page,
                      color=title_of_work),
                  show.legend = F)+
        geom_image(data=max_works, 
                   aes(x=publication_year,
                       y=position,
                       image = image), 
                   size = 0.15)+
        geom_label(data=max_works, 
                   aes(x=publication_year,
                       y=position-0.25,
                       label = label,
                       color=title_of_work),
                   size=3,
                   fill = "white",
                   show.legend = F) +
        ylim(c(0,2.5)) +
        xlim(c(1916,2022)) +
   #     theme(axis.text.x=element_text(angle=45))+
  #      expand_limits(x= c(-1, length(unique(max_works$edition_number_year))))+
        theme_bw() +
        labs(title="Dutch vs. Spanish artists", 
             subtitle="Ratio per page occupied by Dutch and Spanish Artists for different Editions \n with a picture of the biggest per year") +
        xlab("Book's Publication Year")+
        ylab("Ratio of Space per Page")
ggsave("230117.png")
