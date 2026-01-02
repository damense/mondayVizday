library(tidytuesdayR)
library(tidyverse)
library(caret)


# get data ----
tuesdata <- tidytuesdayR::tt_load(2024, week = 53)
tuesdata_df <- tuesdata$book |> 
        full_join(
                tuesdata$broadcast_media,
                by=join_by()
        )
