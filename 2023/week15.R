# 25-Feb-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(jpeg)
library(extrafont)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 15)
egg_production <- tuesdata$`egg-production`
cage_free_percentages <- tuesdata$`cage-free-percentages`
extrafont::font_import()
extrafont::fonts()
# Wrangling
egg_production$month <- month(egg_production$observed_month, label=TRUE)
egg_production$year <- year(egg_production$observed_month)
p1 <- egg_production %>% 
        filter(year<2021,
               prod_type=="hatching eggs") %>%
        group_by(year, month, prod_type) %>%
        summarise(n_henses=sum(n_hens),
                  n_eggses=sum(n_eggs)) %>%
        ggplot() + geom_point(aes(x=n_henses/1e6,
                                 y=n_eggses/1e9,
                                 color=factor(month))) + 
        facet_wrap (prod_type ~year, nrow=1) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, v=0.6),
              strip.background=element_rect(fill="#FEF65B"),
              panel.background = element_rect(fill="white"),
              plot.background = element_rect(fill="#FFFFF6"))+
        xlab(NULL) +
        ylab(NULL) +
        scale_color_viridis_d(name="Month")

p2 <- egg_production %>% 
        filter(year<2021,
               prod_type=="table eggs") %>%
        group_by(year, month, prod_type) %>%
        summarise(n_henses=sum(n_hens),
                  n_eggses=sum(n_eggs)) %>%
        ggplot() + geom_point(aes(x=n_henses/1e6,
                                  y=n_eggses/1e9,
                                  color=factor(month))) + 
        facet_wrap (prod_type ~year, nrow=1) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, v=0.6),
              strip.background=element_rect(fill="#FEF65B"),
              panel.background = element_rect(fill="white"),
              plot.background = element_rect(fill="#FFFFF6"))+
        xlab(NULL) +
        ylab(NULL) +
        scale_color_viridis_d(name="Month")
figure <- ggpubr::ggarrange(p2,p1,ncol=1, common.legend = TRUE, legend = "right",
                            font.label = list(size = 14, color = "black", 
                                              face = "bold", family = "Candara"))
annotate_figure(figure,
                        left=text_grob("Number of Eggs (Billions [1e9])", 
                                       color = "black", 
                                       face = "bold",
                                       rot=90,
                                       size = 14, 
                                       family="Candara"),
                bottom = text_grob("Number of Hens (Millions [1e6])", 
                                        color = "black", 
                                        face = "bold",
                                        size = 14, 
                                        family="Candara"),
                top = text_grob("Evolution of egg production by month, year and type", 
                                   color = "black", 
                                   face = "bold",
                                   size = 20, 
                                   family="Candara")
                )
ggsave("week15.jpg")
