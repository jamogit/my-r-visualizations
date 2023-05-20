library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


URL = "https://raw.githubusercontent.com/owid/owid-datasets/1ae18d982d39e79d0d17b9c266099fc3e2182400/datasets/Gender%20preference%20for%20boss%20-%20Gallup%20(2017)/Gender%20preference%20for%20boss%20-%20Gallup%20(2017).csv"

df <- read.csv(url(URL))


df_p <- pivot_longer(df, cols = !(c("Entity", "Year")))


df_p$group <- ifelse(grepl(".people.", df_p$name) == TRUE, "All", NA)
df_p$group <- ifelse(grepl(".men.", df_p$name) == TRUE, "Men", df_p$group)
df_p$group <- ifelse(grepl(".women.", df_p$name, ignore.case = FALSE) == TRUE, "Women", df_p$group)
df_p$pref <- ifelse(grepl(".male.", df_p$name, ignore.case = FALSE) == TRUE, "Male boss", NA)
df_p$pref <- ifelse(grepl(".female.", df_p$name, ignore.case = FALSE) == TRUE, "Female boss", df_p$pref)






ggplot(data = df_p, aes(x = Year, y = value, colour = pref)) +
  geom_line() +
  facet_wrap(~group) +
  theme_void() +
  labs(
    title = "Gender preference for boss - Gallup 2017 in America",
    subtitle = "If you were taking a new job and had your choice of a boss would you prefer to work for a man or a woman?",
    x = "Year",
    y = "Share of people, %",
    caption = "Data: github.com/owid/owid-datasets   |   #30DayChartChallenge - OWID - Day 6   |   DataViz: @janmoilanen_ek",
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "#FFBA08"),
    axis.text.x = element_text(angle = 50, size = 50, hjust = 1),
    axis.title.x = element_text(size = 60, margin = margin(1, 0, 1, 0, "cm")),
    axis.text.y = element_text(size = 50, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # axis.line = element_line(),
    # axis.ticks = element_line(colour = "#6A040F"),
    panel.grid.major = element_line(colour = "#6A040F"),
    panel.grid.minor.y = element_line(colour = "#6A040F"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#370617"),
    plot.margin = margin(0.5, 1, 0.5, 1, unit = "in"),
    plot.title = element_text(),
    plot.caption = element_text(size = 40),
    plot.subtitle = element_text(size = 40, margin = margin(1, 0, 1, 0, "cm")),
    legend.position = "top",
    legend.text = element_text(size = 60),
    legend.title = element_blank(),
    plot.tag.position = "topright",
    plot.tag = element_text(colour = "#DC2F02", size = 30)
  ) +
  scale_x_continuous(breaks = seq(from = min(df$Year), to = max(df$Year), by = 10))


ggsave("Day6-OWIDPlot-4K.png", units = c("px"), width = 3840, height = 2160)
