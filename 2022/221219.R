# 12-Dec-22
#Author: David Mendez
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(lubridate)



# Read the dataset ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 50)[[1]]
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
pallette <- c("dodgerblue2", 
                           "#E31A1C", # red
                           "green4",
                     "#6A3D9A", # purple
                     "#FF7F00", # orange
                     "black", "gold1",
                     "skyblue2", "#FB9A99", # lt pink
                     "palegreen2",
                     "#CAB2D6", # lt purple
                     "#FDBF6F", # lt orange
                     "gray70", "khaki2",
                     "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
                     "darkturquoise", "green1", "yellow4", "yellow3",
                     "darkorange4", "brown"
)

# tidy the data ----
tuesdata$date <- as.Date(paste0('01/',
                                tuesdata$month, '/',
                                tuesdata$year),
                         format='%d/%m/%Y')
tuesdata$region <- ''
for (i in 1:length(regions)){
        tuesdata[tuesdata$state_abbr %in% regions[[i]][[2]],]$region <- regions[[i]][[1]]
}
tuesdata$change_yoy <- as.numeric(tuesdata$change_yoy)
tuesdata$change_yoy_se <- as.numeric(tuesdata$change_yoy_se)


grouped_data <- tuesdata %>%
        select(date, region, subsector, change_yoy, change_yoy_se) %>%
        group_by(date, region, subsector) %>%
        summarise(across(c(change_yoy, change_yoy_se), mean))

#plot
ggplot(data=grouped_data[grouped_data$date>as.Date('01/01/2020', 
                                              format='%d/%m/%Y') &
                                 grouped_data$date<as.Date('01/07/2021',
                                                      format='%d/%m/%Y'),]) + 
        geom_line(aes(x=date,
                       y=change_yoy,
                      color = region
                       ), linewidth=1) +
        scale_color_manual(values=pallette)+
         facet_wrap (~subsector) +
        theme(axis.text.x = element_text(angle = 35,vjust = 0.7 ,hjust=0.5),
              axis.title.x=element_blank())+
        ylab('Year over Year \n percentage change')
