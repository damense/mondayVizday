# 5Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(igraph)
library(usmap)
library(raster)

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 31)
states <- tuesdata$states
state_name_etymology <- tuesdata$state_name_etymology %>%
        left_join(states %>% select(state, postal_abbreviation))

# adding data
trees <- c("Choctaw","Aleut_Russian","Basque","Oódham_Spanish","Quapaw_Miami-Illinois_French",
           "Spanish","Spanish","Eastern Algonquian","French_English","Spanish",
           "English","Hawaiian","English","Plains Apache","Miami-Illinois",
           "Latin","Dakota_French","Kansa_French","Iroquoian","French",
           "English","French","English","English","Eastern Algonquian",
           "Ojibwe_French","Dakota","Ojibwe_French","Miami-Illinois_French","Spanish",
           "Chiwere","Spanish","English","English","Nahuatl_Spanish",
           "English","Latin_English","Dakota","Seneca_French","Choctaw",
           "Unknown","Latin_English","Dutch","Greek","Latin_English",
           "Dakota","Cherokee","Caddo_Spanish","Apache_Spanish","Ute_Spanish",
           "French","Latin_English","English","Latin_English","Miami-Illinois_French",
           "Delaware")

state_name_etymology$trees <- paste0(trees,"_", state_name_etymology$postal_abbreviation)
edge_list <- data.frame(source=character(0),
                        target=character(0))
for (i in 1:length(trees)){
        lines <- str_split(state_name_etymology$trees[i],"_")[[1]]
        for (j in 2:length(lines)){
                new_df <- data.frame(source=lines[j-1],
                                     target=lines[j])
                edge_list <- rbind(edge_list,
                                   new_df)
        }
        }
cities_t <- usmap_transform(citypop)
plot_usmap()

meta_lang <- data.frame("abbr" = c("Choctaw","Aleut","Russian","Basque","Oódham",
                                   "Spanish","Quapaw","Miami-Illinois","French","Eastern Algonquian",
                                   "English","Hawaiian","Plains Apache","Latin","Dakota",
                                   "Kansa","Iroquoian","Ojibwe","Chiwere","Nahuatl",
                                   "Seneca","Unknown","Dutch","Greek","Cherokee",
                                   "Caddo","Apache","Ute","Delaware"), 
                   "lon" = c(-180, -176, -172, -168, -164, 
                             -160, -156, -152, -148, -144,
                             -140, -136, -132, -128, -124,
                             -120, -116, -112, -108, -104,
                             -100, -96, -92, -88, -84,
                             -80, -76, -72, -68
                             ), 
                   "lat" = c(65.22, 65.22, 65.22, 65.22, 65.22,
                             65.22, 65.22, 65.22, 65.22, 65.22,
                             65.22, 35.22, 65.22, 65.22, 65.22,
                             65.22, 65.22, 65.22, 65.22, 65.22,
                             65.22, 65.22, 65.22, 65.22, 65.22,
                             65.22, 65.22, 65.22, 65.22))
meta <- data.frame("name"=unique(c(edge_list$source, edge_list$target))) %>%
        left_join(rbind(cities_t %>%
                                dplyr::select(abbr, lon, lat),
                        meta_lang), by=c("name"="abbr")) 

g <- graph.data.frame(edge_list, directed = TRUE, vertices = meta)


lo <- layout.norm(as.matrix(meta[,2:3]))

V(g)$label.cex = 0.7
us_map <- getData('GADM', country='US', level=1)
can_map <- getData('GADM', country='CAN', level=1)
dpi <- 1
plot_usmap()
#plot(g, rescale=F, add=T)
plot(g, 
            layout = lo, 
#            add=TRUE,
            rescale = FALSE, 
            edge.curved = TRUE, 
             edge.arrow.size = 0.5, 
             vertex.size = 2, 
             vertex.label.cex = 0.7,
             vertex.frame.color = NA, 
            vertex.label.color = NULL, 
       #  #    edge.color = '#FFFFFF'
       ) 
