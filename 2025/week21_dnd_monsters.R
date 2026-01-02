library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggimage)
library(corrplot)
library(jpeg)
library(cowplot)

# functions ----
#font_import()
loadfonts(device = "win")

#load data

raw_data <- tidytuesdayR::tt_load(last_tuesday()) 
tidytuesdayR::readme(last_tuesday())

monsters <- raw_data$monsters
#img <- jpeg::readJPEG("old-parchment.jpg")





# data exploration

glimpse(monsters)


g <- monsters |> 
  filter(alignment!="Unaligned", type != "Construct") |> 
  mutate(
    alignment= ifelse(alignment=="Neutral","Neutral Neutral",alignment),
    x=gsub("([A-Za-z]+).*", "\\1", alignment),
    y=gsub('^.* ([[:alnum:]]+)$', "\\1", alignment)
    ) |> group_by(x,y, type) |> 
  summarise(Amount=n()) |>
  mutate(x=factor(x, levels = c("Lawful","Neutral","Chaotic")),
         y=factor(y, levels = c("Good","Neutral","Evil")),
         border=1) |> 
  ggplot(
  )+
  geom_tile(
    aes(
      x=x, y=y, fill=Amount
    ), color="black"
  )+
  scale_fill_gradient(low="#d9c57d", high="#4c3e40") +
  theme_minimal()+
  xlab("")+ylab("")+
  facet_wrap(~type, labeller = label_wrap_gen(width=11))+
  ggtitle("Monster Alignment per type")+
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(family="Lucida Handwriting",size = 9),
    legend.title = element_text(family="Lucida Handwriting",size = 11),
    legend.text = element_text(family="Lucida Handwriting"),
    legend.position = "bottom",
    legend.title.position = "top",
    axis.text.x = element_text(angle=25,hjust=1),
    title = element_text(family = "Lucida Handwriting",size = 10,hjust = 0.5)
    )


gc <- ggdraw() +
  draw_image("parchment.png") +
  draw_plot(g,width = 0.5, height = 0.8,x=0.2,y=0.1)

ggsave("week21.jpg")
