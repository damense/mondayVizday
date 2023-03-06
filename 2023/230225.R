# 25-Feb-22
#Author: David Mendez

library(tidyverse)
library(dplyr)
library(tidytuesdayR)
library(afrilearndata)
library(tmap)
library(tmaptools)
library(grDevices)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 09)
afrisenti <- tuesdata$afrisenti
data(africountries)
ring <- function(x,y,outer,inner,  N=100) {
        
        tx <- seq(0, pi, length.out=N)
        top <- cbind(c(x+cos(tx)*outer, x-cos(tx)*inner), c(y+sin(tx)*outer, y+sin(tx)*inner))
        bot <- cbind(c(x-cos(tx)*outer, x+cos(tx)*inner), c(y-sin(tx)*outer, y-sin(tx)*inner))
        all <- rbind(top, bot)
        colnames(all) <- c('x','y')
        return(data.frame(all))
}
lang <- full_join(full_join(tuesdata$languages,
                            tuesdata$language_scripts,
                            multiple = "all"),
                  tuesdata$language_countries,
                  multiple = "all") 
pos <- ring(20,0,45,70)
coord <- data.frame(in_x=c(pos[1:100,1],pos[201:300,1]),
                    in_y=c(pos[1:100,2],pos[201:300,2]),
                    out_x=c(pos[200:101,1],pos[400:301,1]),
                    out_y=c(pos[200:101,2],pos[400:301,2])
                    )
coord$mean_x <- rowMeans(coord[,c(1,3)])
coord$mean_y <- rowMeans(coord[,c(2,4)])

place <- coord[round(seq(1,200,length.out=length(unique(lang$language)))),]


#get map
col <- colorRampPalette(brewer.pal(name="Dark2", n = 8))(length(unique(lang$language)))
africountries$name[africountries$name=="Swaziland"] <- "Eswatini"
ggplot(data=africountries) +
        geom_sf(fill="lightgrey") +
        geom_sf(data = africountries %>%
                        filter(name %in% (lang$country)),
                fill="white")+
   #     geom_polygon(data=ring(20,0,45,70), aes(x=x,y=y), fill="white")+
        theme_void() +
        scale_color_manual(name="Languages", breaks = unique(lang$language))+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background = element_rect(fill = "lightblue",
                                              colour = "lightblue")
        ) + 
        geom_polygon(data=ring(20,0,45,70), aes(x=x,y=y), fill="white") +
        #labs(title="African Languages")

# for (i in 1:length(unique(lang$language))){
#         p <- p+
#                 geom_sf(data = africountries %>%
#                                 filter(name %in% (lang %>%
#                                                           filter(language == unique(lang$language)[i]) %>%
#                                                           dplyr::select(country))),
#                         color=col[i],
#                         fill=NA,
#                         lwd=1)
#                 
# }
p + geom_polygon(data=ring(20,0,45,70), aes(x=x,y=y), fill="white")
p
        
