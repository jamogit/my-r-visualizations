library(eurostat)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210
id <- search_eurostat("Share of renewable energy in gross final energy consumption by sector",
                      type = "table"
)$code[1]

dat <- get_eurostat(id, time_format = "num", type = "label")


countries <- c("Finland",
               "Sweden",
               "Belgium",
               "Germany",
               "Norway",
               "Denmark",
               "Iceland",
               "Estonia",
               "France",
               "European Union - 27 countries (from 2020)")

no_countries <- c("Germany (until 1990 former territory of the FRG)",
                  "Kosovo (under United Nations Security Council Resolution 1244/99)",
                  "European Union - 27 countries (from 2020)",
                  "Norway", "Iceland", "North Macedonia", "Montenegro",
                  "Serbia")

df <- dat |> filter(nrg_bal == "Renewable energy sources" &
                      !(geo %in% no_countries))
df_mean <- dat |>
  filter(nrg_bal == "Renewable energy sources" & geo == "European Union - 27 countries (from 2020)") |> 
  select( time, EU_mean = values )
df <- left_join(df, df_mean)
df <- df |> arrange(desc(values))


df |>
  ggplot() +
  geom_hline(yintercept = 0, colour = "#E9D8A6", linetype = "solid", size = 0.25, show.legend = FALSE) +
  geom_line(aes(x = time, y = values, colour = geo), size = 1.2, show.legend = FALSE) +
  # geom_line(aes(time, EU_mean), colour = "white", size = 0.4, alpha = 0.5) +
  geom_point(data=df |> 
               group_by(geo) |> 
               slice_max(time),
             aes(x = time, y = values, color = geo), shape = 16,
             show.legend = FALSE) +
  # geom_text(data = df |> group_by(geo) |> slice_max(time),
  #           aes(x = time, y = values, label = round(values, 0)),
  #           alpha = 0.5,
  #           vjust = -1,
  #           hjust = 1,
  #           show.legend = FALSE,
  #           size = 12,
  #           fontface = "bold") +
  # geom_text(data=df |> 
  #             group_by(geo) |> 
  #             slice_max(time),
  #           aes(x = time, y = values, color = geo, label = round(values)),
  #           hjust = -0.5, vjust = 0.5, size = 12, fontface = "bold",
  #           show.legend = FALSE) +
  facet_wrap(~forcats::fct_reorder(geo, desc(values))) +
  theme_void() +
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(colour = alpha("#ee9b00", 0.5), linewidth = 0.1)) +
  labs(
    y = "Share, %",
    x = "Year",
    title = "Share of renewable energy in EU countries",
    subtitle = "Share of renewable energy in gross final energy consumption by country and year",
    caption = "Data: Eurostat via {eurostat} package | #30DayChartChallenge - Green Energy - Day 22  |  DataViz: @janmoilanen_ek  | Credits: Gilbert Fontana/r-graph-gallery.com"
  ) +
  theme(
    text = element_text(colour = "#E9D8A6", size = 40, family = "sans-serif"),
    axis.text.x = element_text(angle = 25, size = 30),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 30, hjust = 0),
    axis.title.y = element_text(angle = 90, hjust = 0, margin = margin(1,1,1,1, "cm")),
    panel.grid.major = element_line(colour = "#1e3147", size = 0.5),
    axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(size = 60),
    plot.subtitle = element_text(),
    plot.margin = margin(0,1,0.5,1, "in")
    # plot.caption = element_text(size = 35, margin = margin(1,0, 0, 0, "in")),
    # axis.line = element_line(size = 0.5, colour = "white")
  ) +
  coord_cartesian(clip = "off")

ggsave("Day22-green-energy-4K.png", units = c("px"), width = 3840, height = 2160)

