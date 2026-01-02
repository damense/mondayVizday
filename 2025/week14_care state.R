library(tidyverse)
library(tidytuesdayR)
library(GGally)
library(extrafont)
library(ggimage)
library(flextable)
library(carData)
library(corrplot)

# functions ----
font_import()
loadfonts(device = "win")

#load data

raw_data <- tidytuesdayR::tt_load(last_tuesday()) 
tidytuesdayR::readme(last_tuesday())

care_state <- raw_data$care_state



# data exploration

glimpse(care_state)


care_state |> 
  # left_join(
  #   tibble::rownames_to_column(carData::States, "state")
  # ) |> 
  select(-condition, -measure_name,-footnote, -start_date, -end_date)|> 
  pivot_wider(names_from=measure_id, values_from = score) |> 
  select(-state) |> 
  cor(use="complete.obs") |> 
  corrplot::corrplot(method = "number",diag=F,number.cex = 0.7)
  

care_state |> 
  # left_join(
  #   tibble::rownames_to_column(carData::States, "state")
  # ) |>
  select(-condition, -measure_name,-footnote, -start_date, -end_date)|> 
  filter(grepl("SEP",measure_id)) |> 
  #pivot_wider(names_from=measure_id, values_from = score) |> 
  #filter(grepl("OP_18",measure_id)) |> 
  #mutate(op=str_sub(measure_id,1,6),
  #       type=str_sub(measure_id,8,str_length(measure_id))) |> 
  ggplot(
    aes(
      x=SEP_1,
      y=SEV_SEP_3HR,
      #label=state
      #col=region
    )
  )+
  geom_point()+
  #geom_text()+
  theme_bw()




# plot

pokemon_df |> 
        filter(id<10000,
               type_1 %in% c("fighting","water","steel")) |> 
        group_by(type_1,generation_id) |> 
        summarise(mean_attack=mean(attack),
                  mean_defense=mean(defense)) |>
        mutate(label=ifelse(generation_id==1,type_1,NA),
               label=ifelse(generation_id==2 & type_1=="steel",type_1,label),
               generation_id=factor(generation_id),
               Type=type_1) |> 
        ggplot(aes(x=mean_defense,y=mean_attack, 
                   col=Type, group=Type))+
        ggrepel::geom_label_repel(aes(label=label), force = 100,seed = 3)+
        geom_point(size=2)+
        geom_path(linetype=2, linewidth=0.75)+
        ggrepel::geom_text_repel(aes(label=generation_id))+
        geom_image(data=images, 
                   aes(x=x,y=y,image = url_image),
                   inherit.aes = F, size=0.25,)+
        geom_image(data=data.frame(
                image="C:/Users/dmend/OneDrive/Desktop/logo.png",
                x=180,
                y=62
        ), aes(x=x,y=y,image=image),size=0.1, inherit.aes = F,alpha=0.6)+
        theme_light()+
        theme(legend.position = "none",
              text = element_text(family = "Monaco"),
              panel.grid = element_blank(),
              axis.title = element_text(size = 14),
              plot.title = element_text(size=16,hjust = 0.5),
              plot.subtitle = element_text(size=8,hjust = 0.5),
              axis.text = element_text(size=8))+
        scale_color_manual(values=c("fighting"=pokemon_df |> 
                                            filter(type_1=="fighting") |> 
                                            select(color_1) |> unique() |> pull(),
                                    "water"=pokemon_df |> 
                                            filter(type_1=="water") |> 
                                            select(color_1) |> unique() |> pull(),
                                    "steel"=pokemon_df |> 
                                            filter(type_1=="steel") |> 
                                            select(color_1) |> unique() |> pull()))+
        scale_y_continuous(limits = c(60, 120), breaks = seq(60,120, by = 10))+
        scale_x_continuous(limits = c(50, 180), breaks = seq(50, 180, by = 10))+
        coord_fixed(ratio=1)+
        xlab("Average Defense")+ylab("Average Attack")+
        ggtitle("Change in Attack and Defense of some pokemon types \nthroughout generations (1-7)",
                subtitle = "Source: {pokemon}")+
        labs(caption = "@dmendsev@fixelfed.social")


