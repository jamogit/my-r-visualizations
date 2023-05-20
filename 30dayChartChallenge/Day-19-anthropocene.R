library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


df <- read_csv("global-energy-substitution.csv")

df_pivot <- df |> pivot_longer(!(c(Entity, Code, Year)))

df_pivot <- df_pivot |> 
  mutate(
    value = ifelse( round(value, 0) == 0, NA, value)
  )


ggplot(df_pivot, aes(x = `Year`, y = `value`)) +
  geom_area(aes(fill = `name`, colour = `name`), alpha = 0.5, show.legend = TRUE) +
  facet_grid(rows = vars(name)) +
  theme_void() +
  labs(
    title = "Global energy consumption by Source",
    subtitle = "Humankind's energy hunger will never decline, but it might transform. #EnergieWende",
    x = "Year",
    y = "Consumption of Energy, Terawatt hours",
    caption = "Source: ourworldindata.org/energy-production-consumption | #30DayChartChallenge - Anthropocene - Day 19  |  DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "white"),
    axis.text.x = element_text(size = 30),
    axis.title.x = element_text(size = 40,  vjust = -0.5),
    axis.title.y = element_text(size = 40, vjust = 1),
    axis.text.y = element_text(angle = 25, size = 20, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    panel.grid = element_line(colour = "grey10", size = 0.5),
    axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(),
    plot.subtitle = element_text(),
    plot.caption = element_text(size = 35, hjust = 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    axis.line.y = element_line(size = 0.5, colour = "white")
  ) +
  scale_fill_ordinal()

ggsave("Day19-Anthropocene-4K.png", units = c("px"), width = 3840, height = 2160)
