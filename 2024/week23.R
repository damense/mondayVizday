library(tidytuesdayR)
library(tidyverse)
library(caret)


# get data ----
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
tuesdata_df <- tuesdata$cheeses

# check data ----
 summary(tuesdata_df)
cheese_clean <- tuesdata_df |> 
        mutate(vegan=ifelse(is.na(vegan),F,vegan))