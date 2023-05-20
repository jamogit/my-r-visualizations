library(readr)
library(ggplot2)
library(zoo)
library(tidyr)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

df <- read.csv("Inflation-Unemployment-2011-2023.txt", sep = "\t", dec = ",")

df_pivot <- pivot_longer(df, cols = !Aika)

c <- cor(df$Inflaatio, df$Tyottomyys)

p <- ggplot(df, aes(`Tyottomyys`, `Inflaatio`))
p + geom_point(size = 1, colour = "white") +
  # geom_path(colour = "yellow", linetype = 2, arrow = arrow(), alpha = 0.3) +
  geom_label(label = sort(df$`Aika`), alpha = 0.2, colour = "grey70", vjust = 0.1, hjust = "inward", size = 6) +
  geom_smooth(method = "lm", formula = y~poly(x, 2), se = TRUE, colour = "white", size = 1) +
  theme_void() +
  labs(
    y = "Annual inflation rate, %",
    x = "Unemployment rate, %",
    title = "The Phillips Curve",
    subtitle =
      paste("Negative correlation between inflation rate and unemployment rate in Finland during 2011-2023.",
            "Correlation = ",
            round(c, 2)),
    caption = "Source: Statistics Finland | #30DayChartChallenge - Correlation - Day 20  |  DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "white"),
    axis.text.x = element_text(size = 40),
    axis.title.x = element_text(size = 40,  vjust = 1),
    axis.title.y = element_text(angle = 90, size = 40, vjust = 1),
    axis.text.y = element_text(size = 40, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    panel.grid = element_line(colour = "grey40", size = 0.5),
    axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(),
    plot.margin = margin(1, 1, 1, 1, "in"),
    plot.subtitle = element_text(),
    plot.caption = element_text(size = 35, margin = margin(1,0, 0, 0, "in")),
    axis.line = element_line(size = 0.5, colour = "white")
  ) +
  scale_fill_ordinal()


ggsave("Day20-Correlation-4K.png", units = c("px"), width = 3840, height = 2160)



