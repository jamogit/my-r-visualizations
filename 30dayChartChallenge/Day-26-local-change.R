library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

df.raw <- read_csv("Potential_National_Income_Loss_from_Chronic_Climate_Damages.csv")


df <- df.raw |> filter(Model == "REMIND-MAgPIE" & Damage == "High" & ISO3 == "FIN" & Percentile == 95)


df_pivot <- pivot_longer(df, cols = starts_with("F2"))
plotdata <- df_pivot |> 
  transmute(
    Year = as.integer(stringr::str_sub(name, 2, 5)),
    Value = value,
    Scenario
  )


ggplot(plotdata, aes(x = Year, y = Value, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, colour = "grey20", size = 0.5) +
  theme_minimal() +
  ylim(-20, 0) +
  labs(
    title = "Local potential National Income Loss from chronic climate damage",
    subtitle = "Locality = Finland. Damage estimate = High*. Assessment model = REMIND-MAgPIE**",
    x = "Year",
    y = "Change in National Income, %",
    caption = "Source: IMF - Climate Change Dashboard | #30DayChartChallenge - Local change - Day 26 | DataViz: @janmoilanen_ek
    ",
    tag = "* = Damages calculated at the 95th confidence interval of the estimates.
** = https://www.ngfs.net/sites/default/files/media/2022/11/21/technical_documentation_ngfs_scenarios_phase_3.pdf"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "grey70"),
    axis.text.x = element_text(size = 40, colour = "white"),
    axis.title.x = element_text(size = 40,  vjust = 1),
    axis.title.y = element_text(angle = 90, size = 30, vjust = 1),
    axis.text.y = element_text(size = 40, hjust = 0, margin = margin(0, 10, 0, 10, "pt"), colour = "white"),
    panel.grid = element_line(colour = "grey10", size = 0.5),
    axis.ticks = element_line(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(size = 60),
    plot.margin = margin(0.1, 0, 0.4, 0.4, "in"),
    plot.subtitle = element_text(),
    plot.caption = element_text(size = 35),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 30)
  )

ggsave("Day26-local-change-4K.png", units = c("px"), width = 3840, height = 2160)
