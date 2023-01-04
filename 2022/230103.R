# 03-Jan-23
#Author: David Mendez

library(tidyr)
library(ggplot2)
library(mapSpain)
library(paletteer)
library(sf)


# Read the dataset ----

provs <- esp_get_prov_siane("León")
wet <- esp_get_rivers()
munic <- esp_get_capimun(region = "León")
munic <- left_join(munic,pobmun19, by='name')


# Make the data prettier
munic$wom_per <- munic$women/munic$pob19*100

rivers_leon <- wet[st_intersects(provs,wet)[[1]],]

# plot
ggplot() +
        geom_sf(data=provs) +
        geom_sf(data=munic, aes(fill=wom_per, 
                                size=log10(pob19)),
                pch=21,
                show.legend = "point")+
        geom_sf(data=rivers_leon,color='darkblue')+
        coord_sf(
                xlim = c(-7.1, -4.7),
                ylim = c(42.05, 43.3)
        ) +
        scale_fill_paletteer_c("grDevices::cm.colors",
                               direction=1,
                               "Percentage\nof women\n",
                               limits=c(40,60),
                               labels=c("40%","45%","50%","55%","60%"))+
        scale_size_continuous("Population",
                              labels=c("100","1.000","10.000","100.000"))+
        theme_classic() +
        theme(legend.position = "right",
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background = element_rect(fill = 'white')
        ) +
        ggtitle("Municipalities of León, Spain",
                subtitle="A look at population and gender")
ggsave('230103.png', width = 5.37*1.5,height = 4.32*1.5)
