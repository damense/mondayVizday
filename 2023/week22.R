# 29Jun23
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fonts()

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 22)
centenarians <- tuesdata$centenarians
centenarians$initial <- factor(substr(centenarians$name,1,1))
table(centenarians$initial)
