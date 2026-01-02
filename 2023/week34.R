# 14Aug23
#Author: David Mendez


library(tidyverse)
library(tidytuesdayR)
library(wpp2022)
library(rnaturalearth)

data("pop1dt")

tuesdata <- tidytuesdayR::tt_load(2023, week = 34)
population <- tuesdata$population 
af =  ne_countries(returnclass="sf")
population$distance <- NA


for (i in 1:length(population$coo_name)){
        try(population$distance[i] <- sf::st_distance(af[af$iso_a3==population$coo_iso[i],],
                                                      af[af$iso_a3==population$coa_iso[i],]),
            silent = T)
}

ggplot(data = population, aes(x=distance, y=refugees)) + 
        geom_point() +
        scale_y_log10()+ 
        stat_smooth(method = "lm", 
                    formula = y ~ x, 
                    col = "red")




rel_pop_coo <- population %>%
        group_by(year, coo_name) %>%
        summarise(tot_ref=sum(refugees),
                  tot_ip=sum(idps)) %>%
        left_join(pop1dt %>%
                          dplyr::select(name,year,pop) %>%
                          mutate(year=year-1),
                  by=c("coo_name"="name","year"="year")) %>%
        mutate(left_ref=tot_ref/pop/1e3*100,
               left_ip= tot_ip/pop/1e3*100)
countries_interesting <- rel_pop_coo %>% 
        filter(left_ref>5) %>% 
        ungroup()%>%
        select(coo_name) %>% 
        unique() %>% 
        unlist(use.names = F)

# rel_pop_coa <- population %>%
#         group_by(year, coa_name) %>% 
#         summarise(tot_ref=sum(refugees),
#                   tot_ip=sum(idps)) %>%
#         left_join(pop1dt %>%
#                           dplyr::select(name,year,pop) %>%
#                           mutate(year=year-1),
#                   by=c("coa_name"="name","year"="year")) %>%
#         mutate(left_ref=tot_ref/pop/1e3*100,
#                left_ip= tot_ip/pop/1e3*100)


ggsave("week34.png")        
