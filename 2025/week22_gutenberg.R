library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggimage)
library(plotly)

# functions ----
#font_import()
loadfonts(device = "win")

#load data

raw_data <- tidytuesdayR::tt_load("2025-06-03") 
#tidytuesdayR::readme(last_tuesday())

gutenberg_data <- raw_data$gutenberg_metadata |> 
  left_join(raw_data$gutenberg_authors,
            by=join_by(gutenberg_author_id, author), relationship = "many-to-many") |> 
  left_join(raw_data$gutenberg_languages,
            by=join_by(gutenberg_id, language), relationship = "many-to-many") |> 
  left_join(raw_data$gutenberg_subjects,
            by=join_by(gutenberg_id), relationship = "many-to-many")





# data exploration

glimpse(gutenberg_data)



shelfs_wide <- gutenberg_data |> 
  separate(gutenberg_bookshelf,into = c("shelf_1","shelf_2","shelf_3","shelf_4","shelf_5","shelf_6","shelf_7"), sep=":") 



# 1
label <- c(unique(shelfs_wide$shelf_1))
parent <- rep("",length(label))
value <- shelfs_wide |> group_by(shelf_1) |> summarise(n=n()) |> pull(n)

# 2
lev_2 <- shelfs_wide |>
  filter(!is.na(shelf_2)) |> 
  group_by(shelf_1, shelf_2) |> 
  summarise(n=n())
label <- c(label,
           unique(shelfs_wide$shelf_2)
           )

parent










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
