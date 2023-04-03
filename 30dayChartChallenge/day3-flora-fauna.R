library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(showtext)

font_add_google(name = "Big Shoulders Inline Text", regular.wt = 300, family = "sans-serif")
showtext_auto()

df <- read.csv2("metsavarat.csv", sep = ";", quote = "\"")
df <- df |> filter( !(`ikäluokka` %in% c("Puuton", "Metsämaa yhteensä")))
df$sort <- switch(df$ikäluokka,
                  "Ikäluokka - 1-20" = 1)

luokat <- data.frame(
  `ikäluokka` = as.factor(
      c("Ikäluokka - 1-20",
             "Ikäluokka - 21-40",
             "Ikäluokka - 41-60",
             "Ikäluokka - 61-80",
             "Ikäluokka - 81-100",
             "Ikäluokka - 101-120",
             "Ikäluokka - 121-140",
             "Ikäluokka - 141+"
             )
      ),
  `label` = as.factor(
    c("1-20",
      "21-40",
      "41-60",
      "61-80",
      "81-100",
      "101-120",
      "121-140",
      "141+"
    )
  ),
  sort = c(1, 2, 3, 4, 5, 6, 7, 8)
)

df <- left_join(df, luokat) |> 
  arrange(sort, maakunta)


ggplot(df, aes(y = `VMI.12.13..2017.2021.`, x = fct_reorder(`label`, `sort`), fill = `ikäluokka`)) +
  geom_col(show.legend = F) +
  theme_void() +
  labs(
    title = "Metsäpinta-alat ikäluokittain ja maakunnittain",
    subtitle = "Forest land (1000 ha) by region and age of forest in Finland",
    x = "Ikäluokka (vuotta)",
    y = "Pinta-ala (1000 ha)",
    caption = "Tietolähde: Luke, VMI 12/13 (2017-2021)   |   #30DayChartChallenge - Flora/Fauna - Day 3   |   DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 60, colour = "black"),
    # axis.line = element_line(colour = "#FFF3B0"),
    # axis.title = element_text(size = 12),
    # axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, size = 27, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(angle = 90, hjust = 1, margin = margin (0, 0.3, 0, 0, "cm")),
    axis.title.x = element_text(margin = margin (0.3, 0, 0, 0, "cm")),
    # axis.line.x = element_blank(),
    # axis.line.y = element_blank(),
    axis.ticks = element_line(colour = "#FFF3B0"),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "grey90"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.caption = element_text(size = 30)
  ) +
  # facet_wrap(~maakunta)
  lemon::facet_rep_wrap(~maakunta, repeat.tick.labels = T)


ggsave("MetsavaratPlot-4K.png", units = c("px"), width = 3840, height = 2160)
