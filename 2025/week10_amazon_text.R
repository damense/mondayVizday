library(tidyverse)
library(tidytuesdayR)
library(osmdata)

#load data

raw_data <- tidytuesdayR::tt_load(last_tuesday()) 
tidytuesdayR::readme(last_tuesday())

report_words_clean <- raw_data$report_words_clean



# data exploration
top_words <- report_words_clean |> 
        group_by(word, year) |> 
        summarize(n=n()) |> 
        arrange(desc(n)) |> 
        filter(n>100 & n<150) |> 
        select(word) |> 
        unique() |> unlist(use.names = F)

report_words_clean |> 
        group_by(word, year) |> 
        summarize(n=n()) |> 
        mutate(type= ifelse(word %in% c("billion","million", "stock","tax"),
                            "money",
                            NA)
               ) |> 
        mutate(type= ifelse(word %in% c("december","januari","february","march","april","may",
                                        "june","july","august","september","october","november"),
                            "months",
                            type)
        ) |>
        mutate(type= ifelse(word %in% c("diversity","black","pandemic","bezos"),
                            "other",
                            type)
        ) |>
        mutate(type= ifelse(word %in% c("bezos","trump","biden"),
                            "names",
                            type)
        ) |>
        filter(!is.na(type)) |> 
        mutate(label=ifelse(year==2006,word,NA)) |> 
        mutate(label=ifelse(year==2011 & word=="diversity",word,label)) |> 
        mutate(label=ifelse(year==2020 & word=="pandemic",word,label)) |> 
        ggplot(aes(y=n,x=year,col=word))+
        geom_line(linewidth=1.5)+
        geom_text(aes(label=label),nudge_y = 0.15)+
        theme_light()+
        xlab("")+ylab("Counts")+
        theme(legend.position = "none")+
        scale_y_log10()+
        facet_wrap(~type)+
        ggtitle("Words from Amazon's annual reports")
        
        
        
        report_words_clean |> 
                group_by(word, year) |> 
                summarize(n=n()) |> 
                arrange(desc(n)) |> 
                filter(word %in% c("diversity"))
