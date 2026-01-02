library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggimage)
library(plotly)
library(gganimate)

# functions ----
#font_import()
loadfonts(device = "win")

#load data

raw_data <- tidytuesdayR::tt_load(last_tuesday()) 
#tidytuesdayR::readme(last_tuesday())

weekly_gas_prices <- raw_data$weekly_gas_prices

# data exploration
glimpse(weekly_gas_prices)

weekly_gas_prices |> 
  mutate(month=month(date),
         year=year(date)) |> 
  group_by(year, fuel) |> 
  summarise(n=n())

weekly_gas_prices |> 
  filter(fuel!="diesel",
         date>dmy("01012020"),
         grade=="all") |> 
  pivot_wider(names_from = formulation, values_from = price) |> 
  mutate(diff=(reformulated-conventional)/reformulated) |> 
  ggplot(
    aes(
      x=date,
      y=diff,
      col=grade
    )
  )+
  geom_line()


weekly_gas_prices |> 
  ggplot(aes(
    x=
  ))



ggsave("week21.jpg")
