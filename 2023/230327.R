# 25-Feb-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 12)
languages <- tuesdata$languages
languages$first_letter <- toupper(substr(languages$title, 1,1))



ggplot(data=languages %>% filter(appeared>1800),
       aes(x=appeared,
           y=first_letter)) +
        geom_point()
colnames(languages)

ggplot() + 
        geom_line(data=languages %>% 
                          filter(appeared>1950) %>%
                          group_by(appeared) %>% 
                          summarise(n = n()) %>% 
                          mutate(n = cumsum(n)),
                  aes(x=appeared, y=n),color="blue") +
        geom_line(data=languages %>% 
                          filter(appeared>1950,
                                 is_open_source==T) %>%
                          group_by(appeared) %>% 
                          summarise(n = n()) %>% 
                          mutate(n = cumsum(n)),
                  aes(x=appeared, y=n),color="red") +
        geom_line(data=languages %>% 
                          filter(appeared>1950,
                                 github_language_type=="programming") %>%
                          group_by(appeared) %>% 
                          summarise(n = n()) %>% 
                          mutate(n = cumsum(n)),
                  aes(x=appeared, y=n),color="green") +
        scale_y_log10()





languages %>%
        dim() ## 4303

languages %>%
        filter(is_open_source==T) %>%
        dim() ## 453

languages %>%
        filter(is_open_source==T) %>%
        filter(github_language_type=="programming") %>% ## 159
      #  filter(line_comment_token=="#") %>% ##26
        filter(language_rank<20)%>%
        select(title)%>%
        dim() 

numb <- 50
language_list <- list(
        all =languages %>%
                filter(language_rank<numb)%>%
                select(title) %>%
                unlist(use.names = F),
        open_source=languages %>%
                filter(language_rank<numb)%>%
                filter(is_open_source==T) %>%
                select(title) %>%
                unlist(use.names = F),
        programming_language =languages %>%
                filter(language_rank<numb)%>%
                filter(github_language_type=="programming") %>% ## 159
                select(title)%>%
                unlist(use.names = F)
        # hash_as_comment=languages %>%
        #         filter(line_comment_token=="#") %>%
        #         select(title)%>%
        #         unlist(use.names = F),
        # top_20=languages %>%
        #         filter(language_rank<20)%>%
        #         select(title)%>%
        #         unlist(use.names = F)
)




library(ggVennDiagram)
ggVennDiagram(language_list) + scale_color_brewer(palette = "Paired")
