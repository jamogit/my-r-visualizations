library(monochromeR)
library(ggplot2)
library(dplyr)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


autoplot(df)
#colors()

values <- zoo::fortify.zoo(datasets::sunspot.year)
count_values <- values |> select(`datasets::sunspot.year`) |> distinct() |> arrange(`datasets::sunspot.year`)
palette <- generate_palette("purple", "go_lighter", nrow(values), view_palette = T)
id <- seq(1:nrow(values))


df <- data.frame(
  Id = id,
  Year = values$Index,
  Colour = rev(palette),
  Value = values$`datasets::sunspot.year`
)


ggplot(df, aes(x = Year, y = Value, colour = Colour)) +
  geom_point(colour = df$Colour, size = 2, alpha = 0.5) +
  geom_point(colour = df$Colour, size = 1, alpha = 0.4) +
  geom_point(colour = df$Colour, size = 3, alpha = 0.3) +
  geom_smooth(colour = palette[2], fill = palette[200]) +
  geom_path(colour = df$Colour, alpha = 0.4) +
  theme_void() +
  labs(
    title = "Number of sunspots",
    subtitle = "Yearly number of sunspots from 1700 to 1988",
    x = "Year",
    y = "Number of sunspots",
    caption = "Source: R {datasets} package   |   #30DayChartChallenge - Monochrome - Day 29  |  DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = palette[1]),
    plot.background = element_rect(fill = palette[289]),
    axis.text.x = element_text(angle = 25, size = 40, colour = palette[2]),
    axis.title.x = element_text(size = 40,  vjust = 1),
    axis.title.y = element_text(angle = 90, size = 40, vjust = 0),
    axis.text.y = element_text(size = 40, hjust = 0, margin = margin(0, 10, 0, 10, "pt"), colour = palette[2]),
    panel.grid.major = element_line(colour = palette[200]),
    axis.ticks = element_line(),
    plot.title = element_text(size = 70),
    plot.margin = margin(1,2,1,2, "in"),
    plot.subtitle = element_text(),
    legend.title = element_blank()
  ) +
  scale_x_continuous(breaks = seq(from = 1700, to = 1988, by = 50))

ggsave("Day29-Monochrome-4K.png", units = c("px"), width = 3840, height = 2160)
