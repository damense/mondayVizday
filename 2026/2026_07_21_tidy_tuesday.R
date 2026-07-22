library(tidyverse)
library(tidytuesdayR)

setwd("C:/Users/dmend/OneDrive/Desktop/code/R/mondayVizday/2026")
# Load the weekly Data
tt <- tt_load("2026-07-21")
raw_data <- tt$nde_experiences

# Wrangle
silver_layer <- raw_data |> 
        filter(language == "english",
               classification == "NDE",
               country %in% c("United States", "Canada"))

bool_mat <- silver_layer |> 
        select(contains("ai")) |> 
        as.matrix()

cooc_counts <- t(bool_mat) %*% bool_mat

cooc_df <- as_tibble(cooc_counts, 
                     rownames = "var1") |> 
        pivot_longer(-var1, names_to = "var2", values_to = "cooccurrence") |> 
        filter(var1!=var2) |> 
        mutate(cooccurrence = cooccurrence/sum(cooc_counts)) |> 
        mutate(var1 = replace_values(var1,
                                   "ai_obe" ~ "Out of Body",
                                   "ai_unity" ~ "Unity",
                                   "ai_hellish" ~ "Hellish",
                                   "ai_clinical" ~ "Clinical Death",
                                   "ai_esp" ~ "Extrasensory",
                                   "ai_past_lives" ~ "Past lives",
                                   "ai_world_future" ~ "World Future",
                                   "ai_aliens" ~ "Aliens"
                                   )
               ) |> 
        mutate(var2 = replace_values(var2,
                                 "ai_obe" ~ "Out of Body",
                                 "ai_unity" ~ "Unity",
                                 "ai_hellish" ~ "Hellish",
                                 "ai_clinical" ~ "Clinical Death",
                                 "ai_esp" ~ "Extrasensory",
                                 "ai_past_lives" ~ "Past lives",
                                 "ai_world_future" ~ "World Future",
                                 "ai_aliens" ~ "Aliens"
                                 )
               )

# Visualize --- 
p <- cooc_df |> 
        filter(var1 >= var2) |> 
        ggplot(aes(var1, var2, fill = cooccurrence)) +
        geom_tile(color = "white") +
        geom_text(aes(label = scales::percent(cooccurrence, accuracy = 0.5)), size = 3) +
        scale_fill_gradient(low = "#FFF8E7", high = "#B35806",
                            labels = scales::percent, 
                             name = "Co-occurrence") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              plot.subtitle = element_text(size = 7),
              plot.caption = element_text(size = 6),
              aspect.ratio = 1
              ) +
        labs(x = NULL, y = NULL,
             caption = "Data source: Near Death Experience Research Foundation")+
        ggtitle("Co-ocurrence of Near-Death experiences",
                subtitle = "by percentage of co-ocurrence")

library(cowplot)
library(magick)

logo <- image_read("C:/Users/dmend/OneDrive/Desktop/code/R/mondayVizday/2026/logo.png")

ggdraw(p) +
  draw_image(logo, x = 0.85, y = 0.1, width = 0.1, height = 0.1,
             hjust = 0, vjust = 0)

# Save Image

ggsave(
  filename = "20260721_tt.png",
  device = "png"
)
