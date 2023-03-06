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
library(patchwork)


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
lang$language <- factor(lang$language,
                                 levels = c("Amharic","Tigrinya", "Oromo","Hausa","Nigerian Pidgin",
                                            "Algerian Arabic/Darja","Moroccan Arabic/Darija",
                                            "Twi","Igbo","Yorùbá","Kinyarwanda", "Xitsonga",
                                            "Mozambican Portuguese","Swahili" ))
lang <- lang[order(lang$language),]
pos <- ring(20,0,45,70)
coord <- data.frame(in_x=c(pos[1:100,1],pos[201:300,1]),
                    in_y=c(pos[1:100,2],pos[201:300,2]),
                    out_x=c(pos[200:101,1],pos[400:301,1]),
                    out_y=c(pos[200:101,2],pos[400:301,2])
                    )
coord$mean_x <- rowMeans(coord[,c(1,3)])
coord$mean_y <- rowMeans(coord[,c(2,4)])

place <- coord[round(seq(1,200,length.out=length(unique(lang$language))+1)),c(5,6)]
place$mean_x_nm <- (place$mean_x+60)/150
place$mean_y_nm <- (place$mean_y+75)/150


data_seg <- data.frame(lang=character(0),
                       pos=numeric(0),
                       country=character(0),
                       x=numeric(0),
                       y=numeric(0),
                       xend=numeric(0),
                       yend=numeric(0))
k <- 1
la <- lang$language_iso_code[1]
for (i in 1:dim(lang)[1]){
        if(lang$language_iso_code[i] %in% la){
                
        }else{
                la <- c(la, lang$language_iso_code[i])
                k <- k+1
        }
        coord_temp <- africountries %>% 
                filter(name==lang$country[i]) %>%
                st_centroid() %>% 
                select(geometry) %>% 
                unlist(use.names = F)
        data_seg_temp <- data.frame(lang=lang$language_iso_code[i],
                                    pos=k,
                                    country=lang$country[i],
                                    x=coord_temp[1],
                                    y=coord_temp[2],
                                    xend=place[k,1],
                                    yend=place[k,2])
        data_seg <- rbind(data_seg,
                          data_seg_temp)
}

#get map
africountries$label <-'Positive' 
africountries$label[15] <-'Neutral' 
africountries$label[17] <-'Negative' 
africountries$name[africountries$name=="Swaziland"] <- "Eswatini"

p <- ggplot(data=africountries) +
        geom_sf(fill="lightgrey") +
        
        geom_sf(data = africountries %>%
                        filter(name %in% (lang$country)),
                aes(color=label))+
        
        geom_sf(data = africountries %>%
                        filter(name %in% (lang$country)),
                fill="white")+
      #  geom_blank(aes(color = label), show.legend = TRUE)+
        scale_color_manual(name="Perception",
                           #breaks=c("Positive","Neutral","Negative"),
                           values=c("Positive"="green",
                                    "Neutral"="yellow",
                                    "Negative"="red"))+
        theme_void() +
       # scale_color_manual(name="Languages", breaks = unique(lang$language))+
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
        xlim(-50,90)+
        ylim(-70,70)+
        geom_segment(data=data_seg,
                       aes(x=x,
                           y=y,
                           xend=xend,
                           yend=yend),
                       color="brown",
                       inherit.aes = F) +
        labs(title="Perception of African Languages from tweets")


la <- character(0)
k <- 1
to <- afrisenti %>% group_by(language_iso_code, label) %>% summarise(n=n())
xlim <- max(to$n)
for (i in 1:dim(lang)[1]){
        if(lang$language_iso_code[i] %in% la){
        } else {
                p_temp <-  ggplot()+
                        geom_histogram(data=afrisenti %>% 
                                               filter(language_iso_code==lang$language_iso_code[i]), 
                                       aes(y=label,
                                           fill=label), 
                                       stat="count",
                                       show.legend = F) +
                        scale_fill_manual(values = c("positive"="green",
                                                     "neutral"="yellow",
                                                     "negative"="red"))+
                     #   color = guide_legend(override.aes = list(fill=) ) 
                        theme_void()+
                        xlim(0,xlim)+
                        ggtitle(lang$language[i])+
                        theme(plot.title = element_text(size = 9,hjust = 0.5, face="bold"))
                p <- p + 
                        inset_element(p_temp, left = place[k,3]-0.075, 
                                      bottom = place[k,4]-0.05, 
                                      right = place[k,3]+0.025, 
                                      top = place[k,4]+0.05)
                k <- k+1
                la <- c(la,lang$language_iso_code[i])
                        
        }
        
}

ggsave("230306.png")
