library(rvest)
library(dplyr)
library(stringi)
library(ggplot2)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

# URL <- "https://fi.wikipedia.org/wiki/Luettelo_Suomen_korkeimmista_kohdista"
# 
# vaarat <- URL |>
#   read_html(encoding = "UTF-8") |> 
#   html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]' ) |> 
#   html_table()
# 
# 
# df <- vaarat[[1]]
# write.csv2(df, "vaarat.csv")
# Tässä välissä piti käydä excelissä poistelemassa outoja white-space-koodauksia

library(readr)
vaarat_converted <- read_delim("vaarat_converted.csv", 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                               trim_ws = TRUE)


df <- vaarat_converted |> 
  mutate(
    Region = as.factor(`Sijainti`),
    Height = `Korkeus(m)`,
    Peak =  `Nimi`
  ) |> 
  arrange(desc(`Height`)) |> 
  slice_head(n = 100)

df <- df |> select(Region, Peak, Height) |> arrange(Region, desc(Height)) |> mutate(id = seq(1:nrow(df)))

enontekiorate <- round(nrow(df[df$`Region` == "Enontekiö", ]) / nrow(df) * 100, 0)

group.lkm <- df |> group_by(Region) |> summarise(lkm = n())
df <- inner_join(df, group.lkm)
df$groupindex <- group.sum <- df |> group_by(Region) |> group_indices()


# Get the name and the y position of each label
label_data <- df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)




ggplot(df, aes(x = as.factor(`id`), y = `Height`, fill = `Region`, colour = `Region`)) +
  # geom_bar(stat = "identity",
  #          alpha = 0.9,
  #          linewidth = 0.1,
  #          colour = "#001219") +
    # geom_bar(aes(x = reorder(`Peak`, `Height`), y = Height, fill = Region),
  #          stat = "identity", alpha=0.5, show.legend = F) +
  geom_col(
    position = "dodge2",
    colour = "#001219",
    show.legend = TRUE,
    alpha = 0.9,
    linewidth = 0.2
  ) +
  # geom_text(data = df,
  #           aes(position = "dodge2", x = `Peak`, y = df$Height, label = df$Peak),
  #           show.legend = FALSE) +
  coord_polar(start = 0) +
  ylim(-400, 1550) +
  theme_void() +
  labs(
    title = "Top 100 peaks by Region in Finland",
    subtitle = paste("Enontekiö has the highest peak Halti and", enontekiorate, "% of all the top 100 peaks."),
    x = "Peak",
    y = "Height, meters above sea level",
    caption = "Data: Wikipedia | #30DayChartChallenge - Circular - Day 11 | DataViz: @janmoilanen_ek | Creds: r-graph-gallery.com/297-circular-barplot-with-groups.html",
    # tag = "Powered by R"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "white"),
    # axis.text.x = element_text(size = 30),
    # axis.title.x = element_text(size = 40,  vjust = -0.5),
    # axis.text.y = element_text(size = 30, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    # panel.grid.major.y = element_line(colour = "grey20"),
    axis.text.y = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey20"),
    # axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(),
    plot.subtitle = element_text(),
    plot.caption = element_text(size = 35, hjust = 0),
    plot.tag = element_text(size = 30),
    plot.tag.position = "bottom"
  ) +
  geom_text(data=label_data,
            aes(x = id, y = Height + 10, label = paste(Peak, ",", Height, "m"), hjust = hjust),
            color="grey90",
            size=8,
            family = "sans-serif",
            angle = label_data$angle,
            inherit.aes = FALSE
            ) +
  scale_fill_ordinal()


ggsave("Day11-CircularPlot-4K.png", units = c("px"), width = 3840, height = 2160)
