# 5Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(extrafont)
library(usmap)
library(gganimate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 26)
us_place_names <- tuesdata$us_place_names
us_place_history <- tuesdata$us_place_history
us_place <- us_place_names %>%
        left_join(us_place_history, by=c("feature_id")) 
# us_place$lang <- NA
# us_place$lang_origin <- NA
# euro <- c("Spanish", "French", "German","Italian","English","Dutch",
#           "Polish","Danish","Swedish","Norwegian","Swiss","Czech",
#           "Greek")
# for (i in euro){
#         us_place$lang[grepl(i,us_place$history)] <- i
#         us_place$lang_origin[grepl(i,us_place$history)] <- "European"
# }
# 
# native <- c("Cherokee","Choctaw","Siouan","Nahuatl","Lakota",
#             "Navajo","Zuni","Aymara","Dakota","Muskogean",
#             "Chinook")
# for (i in native){
#         us_place$lang[grepl(i,us_place$history)] <- i
#         us_place$lang_origin[grepl(i,us_place$history)] <- "Native"
# }
data_list <- regmatches(us_place$history, 
                        gregexpr("\\d{4}", 
                                 us_place$history))
us_place$date <- sapply(data_list, paste, collapse=",")
us_place_date <- us_place %>%
        filter(!state_name %in% c("Commonwealth of the Northern Mariana Islands", 
                                  "Guam","American Samoa",
                                  "United States Virgin Islands",
                                  "U.S. Minor Outlying Islands"),
               !is.na(prim_lat_dec),
               !is.na(prim_long_dec)
               )%>%
        mutate(date = strsplit(as.character(date), ",")) %>% 
        unnest(date) %>%
        mutate(date=as.numeric(date))






plot_data <- usmap_transform(
        us_place_date %>%
                filter(!is.na(prim_lat_dec),
                       !is.na(prim_long_dec)),
        input_names = c("prim_long_dec", "prim_lat_dec"),
        output_names = c("x", "y")
)

#fi <- plot_usmap(regions="states")  +
ggplot()+
        geom_point(data=plot_data %>%
                           filter(!is.na(date),
                                  date<2025,
                                  x<3e6,
                                  y<3e6),
                   aes(x=x,
                       y=y)) +
        gganimate::transition_time(time=date, range = c(0,2025)) +
        labs(title = 'Year: {as.integer(frame_time)}')
        #theme(legend.position = "right") +
        #scale_color_discrete(name="Denonyms") +
        #facet_wrap(~lang_origin)

animate(fi,fps=5, nframes=2025)


plot_data <- safi_data[0,]
for (i in 1:dim(safi_data)[1]){
        months <- str_split(safi_data$months_lack_food[i],";")[[1]]
        for (j in months){
                plot_data <- rbind(plot_data,
                                   safi_data[i,] %>%
                                           mutate(months_lack_food=j))
        }
}

plot_data$months_lack_food <- factor(plot_data$months_lack_food,
                                     levels = c("Jan","Feb","Mar","Apr","May","June",
                                                "July","Aug","Sept","Oct","Nov","Dec","none"))
f1 <- plot_data %>%
        group_by(months_lack_food, village) %>%
        summarise(perc=n()/1.31) %>%
        filter(months_lack_food!="none") %>%
        mutate(rel=ifelse(village=="Chirodzo",perc*1.31/39,
                          ifelse(village=="God",perc*1.31/43,
                                 perc*1.31/43))) %>%
        ggplot() +
        geom_col(aes(x=months_lack_food,
                     y=perc,
                     fill=village))+
        ylim(c(0,100))+
        xlab("")+
        ylab("% households\n asked")+
        theme_bw()+
        scale_fill_manual(name="Village",
                          values=c("Chirodzo"="#561217",
                                     "God"="#fbe5b6",
                                     "Ruaca"="#f8b735"))+
        theme(text = element_text(family="Segoe Print"),
              legend.position = c(0.8,0.8))

f2 <- safi_data %>%
        group_by(village) %>%
        summarise(count=n()/1.31) %>%
        mutate(a="All households")%>%
        ggplot()+
        geom_col(aes(x=a,
                     y=count,
                     fill=village))+
        ylim(c(0,100))+
        xlab("")+
        ylab("")+
        theme_bw()+
        theme(axis.text.y = element_blank(),
              text = element_text(family="Segoe Print"),
              legend.position = "none")+
        scale_fill_manual(name="Village",
                          values=c("Chirodzo"="#561217",
                                     "God"="#fbe5b6",
                                     "Ruaca"="#f8b735"))

a1 <- ggarrange(f1,f2, ncol=2, widths = c(1,0.14))

annotate_figure(a1,top = text_grob("\nAmount of households that don't have enough food to feed the members\n",  
                                   size = 16, family = "Segoe Print"))



centenarians <- tuesdata$centenarians
centenarians$initial <- factor(substr(centenarians$name,1,1))

world_coordinates <- map_data("world") %>%
        filter(region == "Japan") %>%
#        mutate(region = ifelse(region == "USA", "United States", region),
#               region = ifelse(region == "Antarctica","", region))%>%
        left_join(data.frame(table(centenarians$place_of_death_or_residence)),
                  by=join_by(region==Var1))
p1 <- ggplot(world_coordinates, 
       aes(x = long, y = lat, group = group)) +
        geom_polygon(fill="Blue") +
        theme_void() +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())


p2 <- ggplot(data=centenarians %>%
               filter(place_of_death_or_residence=="Japan")) +
        geom_histogram(aes(x=age, fill=gender),
                       bins=30) +
        theme_bw() +
        xlab("Age (years)")+
        ylab("Amount")+
        xlim(c(110,120))+
        ylim(c(0,8))+
        theme(legend.position = 'bottom')

 ggarrange(p1,p2,
          NULL,NULL,
          nrow = 2, ncol=2,
          widths = c(1,1.2),
          heights = c(1.2,1))
