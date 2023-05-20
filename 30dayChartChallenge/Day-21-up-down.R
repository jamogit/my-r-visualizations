library(exoplanets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


df <- exoplanets("ps")


df2 <- df |>
  filter( default_flag == 1) |>
  select(
  pl_name,
  disc_year,
  `Discovery method` = discoverymethod
)

df3 <- df2 |> 
  arrange(
    disc_year, `Discovery method`
  ) |> 
  group_by(
    disc_year, `Discovery method`
  ) |> 
  summarise(
    value = n()
  )

df4 <- df3 |>
  group_by(`Discovery method`) |> 
  mutate(
    c_sum = zoo::rollapplyr(value, 121, sum, partial = TRUE)
  )


ggplot(df4, aes(disc_year, c_sum, fill = `Discovery method`)) +
  geom_area(position = "stack", alpha = 0.6) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(
    y = "Count",
    x = "Discovery year",
    title = "Exoplanets discovered",
    subtitle = "Confirmed exoplanets by discovery year and method",
    caption = "Source: NASA via {exoplanets} | #30DayChartChallenge - Down/Upwards - Day 21  |  DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "white"),
    axis.text.x = element_text(size = 40),
    axis.title.x = element_text(size = 40,  vjust = 1),
    axis.title.y = element_text(angle = 90, size = 40, vjust = 1),
    axis.text.y = element_text(size = 40, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    panel.grid = element_line(colour = "steelblue", size = 0.5),
    axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(size = 60),
    plot.margin = margin(1, 1, 1, 1, "in"),
    plot.subtitle = element_text(),
    plot.caption = element_text(size = 35, margin = margin(1,0, 0, 0, "in")),
    # axis.line = element_line(size = 0.5, colour = "white")
  )

ggsave("Day21-Upwards-4K.png", units = c("px"), width = 3840, height = 2160)
