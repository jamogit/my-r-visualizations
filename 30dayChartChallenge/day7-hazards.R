library(devtools)
library(stringr)
library(pxweb)
library(tidyverse)
library(showtext)

font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

pxweb_query_list <- 
  list("Tielaji"=c("SSS"),
       "Nopeusrajoitus"=c("SSS"),
       "Valoisuus"=c("SSS"),
       "Keli"=c("SSS"),
       "Viikonpäivä"=c("SSS","1","2","3","4","5","6","7"),
       "Vuosi"=c("2015","2016","2017","2018","2019","2020","2021","2022","2023"),
       "Tiedot"=c("konn"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/en/StatFin/ton/statfin_ton_pxt_111f.px",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
pxweb_cite(px_data)




df <- px_data_frame |>
  filter( `Day of week` != "Total") |>
  # filter( Year == 2020) |> 
  select(`Year`, Day = `Day of week`, Value = `Personal injury accidents`) |> 
  group_by(Year, Day) |> 
  summarise(Mean = mean(Value))

df$Day <- factor(df$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


ggplot(df, aes(x = Day, y = Mean)) +
  geom_col(fill = ifelse(df$`Day` == "Friday", "#EF233C", "#8D99AE")) +
  geom_text(label = df$Mean, size = 10, nudge_y = 50, colour = "#2B2D42") +
  facet_wrap(~Year) +
  theme_void() +
  labs(
    title = "Road injury accidents between 2015-2023* in Finland",
    subtitle = "Fridays stand out in the statistics on road injury accidents.",
    x = "Weekday",
    y = "Accidents",
    caption = "Data: Statistics Finland   |   #30DayChartChallenge - Hazards - Day 7   |   DataViz: @janmoilanen_ek",
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "#2B2D42"),
    axis.text.x = element_text(angle = 50, size = 30, hjust = 1),
    # axis.title.x = element_text(size = 60, margin = margin(1, 0, 1, 0, "cm")),
    # axis.text.y = element_text(size = 50, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # axis.line.y = element_line(),
    # axis.ticks = element_line(colour = "#6A040F"),
    # panel.grid.major = element_line(colour = "#2B2D42"),
    # panel.grid.major.y = element_line(colour = "#2B2D42"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#EDF2F4"),
    plot.margin = margin(0.5, 1, 0, 1, unit = "in"),
    plot.title = element_text(),
    plot.caption = element_text(size = 40, margin = margin(2, 0, 0, 0, "cm")),
    plot.subtitle = element_text(size = 40, margin = margin(0, 0, 1, 0, "cm")),
    # legend.position = "top",
    # legend.text = element_text(size = 60),
    # legend.title = element_blank()
  )


ggsave("Day7-HazardPlot-4K.png", units = c("px"), width = 3840, height = 2160)
