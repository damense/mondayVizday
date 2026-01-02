library(tidyverse)
library(tidytuesdayR)
library(osmdata)

#load data

raw_data <- tidytuesdayR::tt_load(last_tuesday()) 
tidytuesdayR::readme(last_tuesday())
pixar_films <- raw_data$pixar_films
public_response <- raw_data$public_response


# data exploration
pixar_data <- left_join(pixar_films,
                        public_response,
                        by=join_by(film))

pixar_data |> 
        ggplot( aes(x=rotten_tomatoes,
                    y=metacritic, colour = cinema_score))+
        geom_point()+
        geom_line(data = data.frame(
                rotten_tomatoes=c(40,100),
                metacritic=c(40,100)),
                aes(x=rotten_tomatoes, y=metacritic), 
                linetype=2,
                col="black"
        )+theme_bw()

pixar_data |> 
        mutate(cinema_score=factor(cinema_score, levels = c("A+","A","A-")),
               group=trimws(gsub(pattern = "[1-9]","",film)),
               group = ifelse(film=="Finding Dory","Finding Nemo",group),
               group = ifelse(film=="Incredibles 2","The Incredibles",group),
               group = ifelse(film=="Monsters University","Monsters, Inc.",group)) |>
        rename("Film Rating"="film_rating") |> 
        drop_na(metacritic) |> 
        ggplot( aes(y=metacritic,
                    x=release_date, 
                    
                    label=film))+
        geom_point(aes(colour = `Film Rating`),size=3)+
        geom_line(aes(group=group), linetype=2, alpha=0.3)+
        ggrepel::geom_text_repel(size=3)+
        scale_color_manual(values=c("G"="#10A1AF","PG"="#0D103A"))+
        ylim(c(55,100))+
        xlab("Release date")+ylab("Metacritic Score")+
        ggtitle("Pixar through the years")+
        ggthemes::theme_pander()+
        labs(caption = "https://pixelfed.social/dmendsev")
