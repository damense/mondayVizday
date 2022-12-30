# 26-Dec-22
#Author: David Mendez

library(tidyr)
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(spData)
library(purrr)




# Read the dataset ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 51)[[1]]
regions <- list(list('Alaska and Hawaii', 
                     c('AK','HI')),
                list('West Coast',
                     c('WA','OR','CA')),
                list('Rocky Mountains',
                     c('MT','ID','WY','UT','CO')),
                list('Southwest',
                     c('NV','AZ','NM','TX')),
                list('Great Plains',
                     c('ND','SD','NE','KS','OK')),
                list('Midwest',
                     c('MN','IA','MO','WI','IL','MI','IN','OH')),
                list('South',
                     c('AR', 'LA', 'MS', 'AL', 'TN', 'KY', 'WV', 'DC', 'VA', 'NC', 'SC', 'GA', 'FL')),
                list('Mid-Atlantic',
                     c('PA', 'NY', 'NJ', 'DE', 'MD')),
                list('New England',
                     c('ME', 'VT', 'NH', 'MA', 'CT', 'RI')),
                list('US',
                     c('USA'))
)

# work with the data------
tuesdata$region <- ''

for (i in 1:length(regions)){
        tuesdata[tuesdata$state %in% regions[[i]][[2]],]$region <- regions[[i]][[1]]
        

        }

clean_data <- tuesdata
clean_data$observed_temp_c <- 5/9*(clean_data$observed_temp-32)
clean_data$forecast_temp_c <- 5/9*(clean_data$forecast_temp-32)
clean_data$deltaT <- abs(clean_data$observed_temp_c - clean_data$forecast_temp_c)




# plot
plot_data <- clean_data %>%
        filter(!is.na(region) & 
                       region!="" &
                       !is.na(deltaT) &
                       deltaT<5) 
quantiles <- data.frame(region=character(0),
                        '50%'=numeric(0),
                        '90%'=numeric(0),
                        '95%'=numeric(0),
                        '99%'=numeric(0),
                        'cnt'=numeric(0))


for (i in 1:(length(regions)-1)){
        quantiles[i,1] <- regions[[i]][[1]]
        quantiles[i,2:5] <- quantile(unlist(plot_data %>%  
                                                  filter(region == regions[[i]][[1]]) %>% 
                                                  select(deltaT)), 
                                   c(.5, .9, .95, .99))
        
}

ggplot(data=plot_data)+
        geom_histogram(aes(deltaT,
                          fill=region), 
                       bins = 10,
                       show.legend=F) +
        geom_vline(data=quantiles, aes(xintercept=X50.), linetype=2 )+
        geom_text(data=quantiles, 
                  aes(x=X50. + 0.4, 
                      y=40000),
                  label="50%",
                  size=2.5)+
        geom_vline(data=quantiles, aes(xintercept=X90.), linetype=2 )+
        geom_text(data=quantiles, 
                  aes(x=X90. + 0.4, 
                      y=35000),
                  label="90%",
                  size=2.5)+
        geom_vline(data=quantiles, aes(xintercept=X95.), linetype=2 )+
        geom_text(data=quantiles, 
                  aes(x=X95. + 0.4, 
                      y=40000),
                  label="95%",
                  size=2.5)+
        geom_vline(data=quantiles, aes(xintercept=X99.), linetype=2 )+
        geom_text(data=quantiles, 
                  aes(x=X99. + 0.4, 
                      y=35000),
                  label="99%",
                  size=2.5)+
        facet_wrap(~region) +
        ggtitle("Prediccion innaccuracy",
                subtitle = 'with Percentiles') +
        xlab("Prediction inaccuracy \n(degrees Celsius)")+
        ylab("Amount of predictions\n")+
        theme_classic() +
        theme(text=element_text(family = "sans"))
ggsave("221226.png")
