# 21-Nov-22
#Author: David Mendez

library(tidyr)
library(seab)



# Read the dataset ----
raw_data <- read.csv('C:/Users/david/Desktop/R/mondayVizday/2022/data/dutch_house_prices_dataset.csv')

#clean de dataset----
clean_data <- raw_data 
clean_data$Lot.size..m2. <- gsub(" m²","",
                                 gsub("\\.","",
                                      raw_data$Lot.size..m2.
                                      )
                                 )
clean_data$Living.space.size..m2. <- gsub(" m²","", raw_data$Living.space.size..m2.)
clean_data$Price <- gsub("€ ","",
                         gsub("Prijs op aanvraag","",
                              gsub("\\.","",
                                   raw_data$Price
                                   )
                              )
                         )
clean_data$Estimated.neighbourhood.price.per.m2 <- gsub("€ ","",
                                                        gsub("\\.","",
                                                             raw_data$Estimated.neighbourhood.price.per.m2
                                                             )
                                                        )
clean_data$Build.year <- gsub("Na ","",
                              gsub("Voor ","",
                                   raw_data$Build.year))
clean_data$Energy.label <- gsub("\\+","",raw_data$Energy.label)

# Assign data classes ----
numeric <- c("Price","Lot.size..m2.","Living.space.size..m2.","Estimated.neighbourhood.price.per.m2","Build.year")
fact <- c("Energy.label","Build.type")

proper_data <- clean_data
proper_data[,numeric] <- apply(clean_data[,numeric], 2, function(x) as.numeric(x))
proper_data[,fact] <- apply(clean_data[,fact], 2, function(x) as.factor(x))
proper_data <- proper_data[!is.na(proper_data$Price) & proper_data$Price<1e6 & proper_data$Energy.label != "Niet verplicht",]
 
 #plot
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 ggplot(proper_data, aes(x = Price/1e5, color=Energy.label, fill=Energy.label)) +
         geom_histogram( position="identity",alpha=0.5)+
         facet_grid(. ~ Energy.label) +
         theme(axis.text.x = element_text(angle = 35,vjust = 0.7 ,hjust=0.5),
               legend.position = "none") +
         ggtitle("Distribution of Houses for sale in the Netherlands (<1M€) by Energy Label and Price") +
         xlab("Price (x100.000€)") + ylab("Amount") +
         scale_fill_manual(values=cbPalette) +
         scale_color_manual(values=cbPalette)
 