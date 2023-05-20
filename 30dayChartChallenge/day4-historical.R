library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

font_add_google(name = "Ballet", regular.wt = 400, family = "sans-serif")
showtext_auto()

source <- "Lähde – Källa – Source: SVT: Tilastokeskus, kansantalouden vuositilinpito – FOS: Statistikcentralen, nationalräkenskaper, årsvis – OSF: Statistics Finland, annual national accounts"

df <- read_excel("vuosikirja_2022_kansantalous_02.xlsx",
                 col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

df |>  pivot_longer(cols = c("Alkutuotanto", "Jalostus", "Palvelut"), names_to = "Toimiala") -> df_pivot
df_pivot$Colour <- ifelse(df_pivot$Toimiala == "Alkutuotanto", "yellow",
                          ifelse(df_pivot$Toimiala == "Jalostus", "grey50", "red"))



ggplot(df_pivot, aes(x = Vuosi, y = value, colour = Toimiala)) +
  geom_point(size = 1, shape = 21, stroke = 0.5) +
  geom_line(size = 0.2, alpha = 1) +
  labs(
    title = "Talouden rakennemuutos vuosina 1860-2021",
    subtitle = "Structural change of the Finnish economy between 1860-2021",
    x = "Vuosi",
    y = "Osuus bruttoarvonlisäyksestä, %",
    caption = "Tietolähde: Tilastokeskus | #30DayChartChallenge - Historical - Day 4 | DataViz: @janmoilanen_ek, Github: @jamogit | Google Font: Ballet"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "sans-serif", size = 100, colour = "black"),
    axis.text.x = element_text(angle = 50, size = 50, hjust = 1),
    axis.title.x = element_text(size = 60, hjust = 1, margin = margin(10, 0, 10, 0, "pt")),
    axis.text.y = element_text(size = 50, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(10, 10, 0, 0, "pt")),
    axis.line = element_line(),
    axis.ticks = element_line(colour = "#FFF3B0"),
    panel.grid = element_line(colour = "grey80"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "antiquewhite"),
    plot.margin = margin(0.5, 1, 0.5, 1, unit = "in"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 40, colour = "grey50"),
    plot.subtitle = element_text(hjust = 0.5, size = 60, margin = margin(0, 0, 0, 0, "cm")),
    legend.position = "top",
    legend.text = element_text(size = 60),
    legend.title = element_blank()
  ) +
  scale_x_continuous(breaks = seq(from = 1860, to = 2020, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0, 75))

ggsave("Day4-HistoricalPlot-4K.png", units = c("px"), width = 3840, height = 2160)
