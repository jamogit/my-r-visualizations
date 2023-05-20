library(WDI)
library(monochromeR)
library(ggplot2)
library(dplyr)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

dat = WDI(indicator='FP.CPI.TOTL.ZG', start = 1960, end = NULL)
countries <- dat |> select(`country`) |> distinct()
dat$country <- as.factor(dat$country)


palette <- generate_palette("purple", "go_lighter", nrow(countries), view_palette = T)

df <- dat |> 
  filter(
    year == 2021
  ) |>
  transmute(
    Country = `country`,
    Value = `FP.CPI.TOTL.ZG`,
    Colour = palette
  ) |> 
  arrange(desc(Value)) |> 
  filter(
    !is.na(Value)
  ) |> 
  top_n(50)



ggplot(df, aes(x = forcats::fct_reorder(Country, Value), y = Value)) +
  geom_col(aes(colour = df$colour), show.legend = FALSE) +
  theme_void() +
  labs(
    title = "Top 50 inflationary countries in 2020",
    subtitle = "Annual consumer price change (%)",
    y = "Annual inflation, %",
    caption = "Source: World Bank via {WDI} R package | #30DayChartChallenge - World Bank Data - Day 30 | DataViz: @janmoilanen_ek"
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "sans-serif", size = 30, colour = "white"),
    axis.text.x = element_text(angle = 45, size = 20, vjust = 0),
    axis.title.x = element_text(size = 30,  hjust = 1, margin = margin(1, 1, 1, 1, "cm")),
    axis.text.y = element_text(size = 19, hjust = 1),
    panel.grid.major.y = element_line(colour = "grey20"),
    panel.grid.major.x = element_line(colour = "grey20"),
    plot.margin = margin(0.1, 1, 1, 1, "in"),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 25, hjust = 0)
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 400, by = 10))

ggsave("Day30-World-Bank-data-4K.png", units = c("px"), width = 3840, height = 2160)

