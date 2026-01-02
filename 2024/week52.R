library(tidytuesdayR)
library(tidyverse)
library(gapminder)
library(scales)
library(countrycode)


# get data ----
tuesdata <- tidytuesdayR::tt_load(2024, week = 52)
global_holidays <- tuesdata$global_holidays
monthly_passengers <- tuesdata$monthly_passengers

# process data ----

top_int_tourists <- monthly_passengers |> 
        filter(Year==2018) |> 
        group_by(ISO3) |> 
        summarise(year_int=sum(International,na.rm = T)) |> 
        arrange(desc(year_int)) |> 
        slice(1:4)

top_dom_tourists <- monthly_passengers |> 
        filter(Year==2018) |> 
        group_by(ISO3) |> 
        summarise(year_dom=sum(Domestic,na.rm = T)) |> 
        arrange(desc(year_dom)) |> 
        slice(1:4)

monthly_passengers_plus <- monthly_passengers |> 
        mutate(date=dmy(paste("01-",Month,"-",Year)),
               class=ifelse(ISO3 %in% top_int_tourists$ISO3,
                            "High international tourism",
                            NA),
               class=ifelse(ISO3 %in% top_dom_tourists$ISO3,
                            "High domestic tourism",
                            class),
               Country=factor(countrycode(ISO3, "iso3c", "country.name"),
                           levels = countrycode(c(top_dom_tourists$ISO3,
                                                  top_int_tourists$ISO3), 
                                                "iso3c", 
                                                "country.name")
                           )
               )

public_holidays <- global_holidays |> 
        filter(
               Type=="Public holiday") |> 
        mutate(Year=year(Date),
               Month=month(Date)) |> 
        group_by(ISO3, Year, Month) |> 
        summarise(holidays=n())

## plot data ----
ggplot(data=monthly_passengers_plus |>
       filter(!is.na(class),
              #Year==2018
              ) |>
               pivot_longer(cols = c(Domestic, International)) |>
               filter(!name==""),
       aes(x=date, y=value, col=Country))+
        geom_line()+
        theme_bw()+
        scale_y_log10()+
        facet_grid(name~class)+
        theme(axis.text.x = element_text(angle=35, hjust=1))+
        ylab("")+xlab("")+
        guides(col=guide_legend(ncol=2))+
        ggtitle("Monthly Tourists",
                subtitle = "Highest domestic and international destinations")+
        theme(legend.position = "bottom",
              legend.spacing = unit(0, "mm"), 
              legend.box.background = element_rect(colour = "black",linewidth = 1),
              plot.background = element_rect(fill="#fff9d4",color="black",linewidth = 2),
              strip.background = element_rect(fill = "white"),
              aspect.ratio = 2/3.5,
              text = element_text(size=14))
ggsave("2024/week52.jpg")

# ggplot(data=monthly_passengers_plus |> 
#                left_join(public_holidays,
#                          by=join_by(ISO3,Year, Month))|>
#                filter(ISO3 =="ESP"),
#        aes(x=Month, y=Domestic))+
#         geom_line(aes(col=Year, group=Year))+
#         geom_point(aes(size=holidays))+
#         scale_x_continuous(breaks=seq(1,12,1))+
#         scale_size_continuous(breaks=c(1,2,3), 
#                               range = c(1,2))
