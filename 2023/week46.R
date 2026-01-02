library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(terra)
library(sf)
library(fs)
library(rdgal)
library(patchwork)

download.file("http://biogeo.ucdavis.edu/data/diva/adm/IND_adm.zip", 
              destfile = "IND_adm.zip")
unzip("IND_adm.zip", overwrite = TRUE)

# Getting all file paths
shapefiles <- "C:/Users/dmend/OneDrive/Desktop/code/R/mondayVizday/2023" |>
        dir_ls(recurse = TRUE, regexp = 'shp$') 

# Loading all files
sfdf <- shapefiles |>
        map(st_read) |>
        bind_rows() %>%
        left_join()

# Connectors

center <- c(82,21)
radius <- 25
coord <- data.frame(to_x=numeric(0),to_y=numeric(0))
for (i in seq(0,2*pi-2*pi/16,2*pi/16)){
        coord <- rbind(coord,
                       data.frame(to_x=center[1]+radius*cos(i),
                                  to_y=center[2]+radius*sin(i)))
}
connectors <- cbind(data.frame(State=c("Andhra Pradesh","Bihar", "Delhi", "Gujarat", "Haryana", "Himachal Pradesh",
                                                     "Jharkhand", "Karnataka","Kerala", "Madhya Pradesh", "Maharashtra", "Punjab",          
                                                     "Rajasthan","Telangana","Uttar Pradesh","Uttarakhand"),
                                             from_x=c(79,86,77,72.5,76,77,
                                                      85.5,76,76.5,78,76,75,
                                                      73,79.5,81,79.5),
                                             from_y=c(15,26.5,28,23,29,32,
                                                      24,14,10,24,19,31,
                                                      26,17,26,30)), 
                    coord[c(14,2,5,10,8,6,
                            1,12,13,16,11,7,
                            9,15,3,4),])
# check they've loaded correctly with a plot
main <- ggplot(data=sfdf %>% 
                       filter(TYPE_1 %in% c("State","Union Territor")))+ 
        geom_sf(fill="white")+
        geom_sf(data=sfdf %>% 
                        filter(TYPE_1 %in% c("State","Union Territor"),
                               NAME_1 %in% c("Himachal Pradesh","Punjab","Uttaranchal","Haryana","Delhi",
                                             "Rajasthan","Uttar Pradesh","Bihar")),
                fill="orange")+
        geom_sf(data=sfdf %>% 
                        filter(TYPE_1 %in% c("State","Union Territor"),
                               NAME_1 %in% c("Andhra Pradesh", "Karnataka","Kerala",       
                                             "Telangana")),
                fill="lightgreen")+
        geom_sf(data=sfdf %>% 
                        filter(TYPE_1 %in% c("State","Union Territor"),
                               NAME_1 %in% c(NA,"Andaman and Nicobar",    "Arunachal Pradesh" ,     "Assam"   ,               "Chandigarh"   ,         
                                             "Chhattisgarh",   "Dadra and Nagar Haveli", "Daman and Diu" ,         "Goa"  ,                  "Jammu and Kashmir"   ,  
                                             "Lakshadweep", "Manipur"    ,            "Meghalaya" ,             "Mizoram"  ,              "Nagaland"  ,            
                                             "Orissa", "Puducherry"  ,           "Sikkim"        ,        "Tamil Nadu"   ,          "Tripura"        ,       
                                              "West Bengal" )),
                fill="grey")+
        geom_segment(data=connectors, aes(x=from_x,
                                          y=from_y,
                                          xend=to_x,
                                          yend=to_y),
                     linewidth=0.5,
                     linetype=2)+
        ylim(c(-10,50))+
        theme_minimal()+
        xlab("")+ylab("")+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none")
        

tuesdata <- tidytuesdayR::tt_load(2023, week = 46) 
sum_data <- tuesdata$diwali_sales_data %>%
        group_by(`Age Group`,State, Gender) %>%
        summarise(amount=n()) %>%
        mutate(amount=ifelse(Gender == "M", -amount, amount))
p <- main+ggtitle("Retail Sales during Duwali",
                  subtitle = "For different States, Age groups and genders")+
                  labs(caption= "Age groups are(bottom to top): 0-17, 18-25, 26-35, 36-45, 46-50, 51-55, 55+\n Genders are Femlae in salmon, Male in blue")+
        theme(plot.caption = element_text(hjust = 1),
              plot.caption.position = "plot")
for (i in 1:length(connectors$State)) {
        p_temp <- ggplot(data=sum_data %>%
                                 filter(State==connectors$State[i]), 
                         aes(x=amount,y=`Age Group`, fill=Gender))+ 
                geom_col()+
                theme_bw()+
                ggtitle(connectors$State[i])+
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(size = 9,hjust = 0.5, face="bold"))+
                xlim(c(min(sum_data$amount),max(sum_data$amount)))
        kx <- (connectors$to_x[i]-58)/(107-58)
        ky <- (connectors$to_y[i]+10)/(60)
        p <- p+inset_element(p_temp, left = kx-0.075, 
                           bottom = ky-0.075, 
                           right = kx+0.075, 
                           top = ky+0.075)
}
p

ggsave("week46.png")
       

 