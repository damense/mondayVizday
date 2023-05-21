# 01 - May-23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
#library(ggpubr)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 20)
tornados <- tuesdata$tornados

ggplot(data=tornados,
       aes(x=datetime_utc)) +
        geom_histogram(fill=NULL) +
        theme_bw()
