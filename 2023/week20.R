# 01 - May-23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(usmap)
library(extrafont)
library(gganimate)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 20)
tornados <- tuesdata$tornados

temp_tor <- usmap_transform(tornados,
                                  input_names = c('slon','slat'),
                                  output_names = c('sx','sy'))
plot_data <- usmap_transform(temp_tor,
                              input_names = c('elon','elat'),
                              output_names = c('ex','ey')) %>%
        filter(elon!=0,
               st %in% states2plot$state,
               st!="HI",
               mag>2) %>%
        mutate(mag=factor(mag))

states2plot <- data.frame(state=c('MN','IA','MO','AR','LA','KY','TN'),
                          fill=c('white','pink','white','white','lightgrey','beige','white'),
                          )



myColors <- brewer.pal(length(unique(plot_data$mag)), name="Blues")
names(myColors) <- levels(plot_data$mag)
custom_colors <- scale_colour_manual(name = "Magnitude", values = myColors)

p1 <-plot_usmap(include = states2plot$state, data = states2plot, values = "fill") +
        scale_fill_manual(guide='none',
                values = c(white = "white", pink = "pink", lightgrey = "lightgrey", beige = "beige")
        )+
        geom_segment(data = plot_data %>%
                             mutate(tempvar="Tornados in the states that form the KFC chef"),
                  aes(x=sx,
                      y=sy,
                      xend=ex,
                      yend=ey,
                      color=factor(mag)), 
                  linewidth=1.5) +
        xlim(c(-6e5,25e5))+
        theme(legend.position = c(0.8,0.5),
              strip.text = element_text(size = 14, face = "bold", hjust=0.5),
              panel.background = element_rect(fill = 'white',color='white'),
              strip.background = element_rect(fill="white", color='white')) +
        facet_grid(. ~ tempvar) +
        custom_colors 
#        ggtitle("Tornados in the states that form the KFC chef")


plots_states <- list()
for (i in states2plot$state){
        plots_states[[i]] <- ggplot(data=tornados %>%
                                            filter(st ==i),
                                    aes(x=datetime_utc)) +
                geom_histogram(fill='white',
                               color='black')+
                theme_minimal() +
                ylim(c(0,200))+
                xlab('') +
                ylab('') +
                theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                      axis.text.x = element_text(angle=45))
}

p1 + annotation_custom(ggplotGrob(plots_states$MN), xmin = -5e5, xmax = 2e5, 
                       ymin = -2.5e5, ymax = 3.5e5) +
        annotation_custom(ggplotGrob(plots_states$IA), xmin = -5e5, xmax = 2e5, 
                          ymin = -7.5e5, ymax = -1.5e5)+
        annotation_custom(ggplotGrob(plots_states$MO), xmin = -4e5, xmax = 3e5, 
                          ymin = -11.5e5, ymax = -5.5e5) +
        annotation_custom(ggplotGrob(plots_states$AR), xmin = -3e5, xmax = 4e5, 
                          ymin = -15.5e5, ymax = -9.5e5) +
        annotation_custom(ggplotGrob(plots_states$LA), xmin = -2e5, xmax = 5e5, 
                          ymin = -19.5e5, ymax = -13.5e5) +
        annotation_custom(ggplotGrob(plots_states$KY), xmin = 10e5, xmax = 17e5, 
                          ymin = -7e5, ymax = -1e5) +
        annotation_custom(ggplotGrob(plots_states$TN), xmin = 15e5, xmax = 22e5, 
                          ymin = -14e5, ymax = -8e5)
ggsave('week20.png')
