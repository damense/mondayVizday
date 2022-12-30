# 26-Dec-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)
library(ggwordcloud)
library(rtrek)
library(showtext)
library(trekfont)




# Read the dataset ----

transcripts <- st_transcripts()

transcripts$char <- ''
for (i in 1:dim(transcripts)[1]){
        transcripts$char[i] <- paste(transcripts$text[[i]]$character, 
                                     collapse = ' ')
}
char <- data.frame(order=integer(0),
                   char=character(0),
                   series=character(0))

for (i in 1:length(unique(transcripts$series))){
        ser <- unique(transcripts$series)[i]
        total_char <- paste(transcripts[transcripts$series==ser,]$char, 
                            collapse = ' ')
        total_char_freq <- as.data.frame(table(str_split(total_char," ")))
        total_char_freq$Freq <- total_char_freq$Freq/sum(total_char_freq$Freq)
        top_char_freq <- head(total_char_freq[order(-total_char_freq$Freq),],20)
        new_char <- data.frame(order=top_char_freq$Freq,
                               char=top_char_freq$Var1)
        new_char$series <- ser
        char <- rbind(char, new_char)
}


char$series[char$series=="TOS"] <- "The Original \nSeries"
char$series[char$series=="TAS"] <- "The Animated \nSeries"
char$series[char$series=="TNG"] <- "The Next \nGeneration"
char$series[char$series=="DS9"] <- "Deep Space \nNine"
char$series[char$series=="VOY"] <- "Voyager"
char$series[char$series=="ENT"] <- "Enterprise"
char$series <- factor(char$series, c("The Original \nSeries",
                                     "The Animated \nSeries",
                                     "The Next \nGeneration",
                                     "Deep Space \nNine",
                                     "Voyager",
                                     "Enterprise"))
## plot

ggplot(data=char, 
       aes(label = char, size=order, color=series)) +
        geom_text_wordcloud() + 
        scale_size_area(max_size = 12)+     
        theme_minimal() +
        facet_wrap(~series) +
        theme(panel.background = element_rect(fill = 'black'),
              strip.background=element_rect(fill="darkblue"),
              strip.text = element_text(colour = 'white',
                                        family="Times", 
                                        face="bold", 
                                        size=10))
ggsave("221230.png")
