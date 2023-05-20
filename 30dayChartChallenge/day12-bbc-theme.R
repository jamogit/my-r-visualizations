library(pxweb)
library(ggplot2)
library(bbplot)
library(zoo)
library(dplyr)
library(readr)
library(extrafont)
library(showtext)
font_add_google(name = "Open Sans", regular.wt = 400, family = "sans-serif")
showtext_auto()


# PXWEB query 
# pxweb_query_list <- 
#   list("Verokausi"=c("*"),
#        "Verotuskunta"=c("*"),
#        "Erä"=c("TRT_TULOT_10"),
#        "Tunnusluvut"=c("Arvo_sum"))
# 
# # Download data 
# px_data <- 
#   pxweb_get(url = "http://vero2.stat.fi/PXWeb/api/v1/en/Vero/Tulorekisteri/trt_010.px",
#             query = pxweb_query_list)
# 
# # Convert to data.frame 
# px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
# 
# # Get pxweb data comments 
# px_data_comments <- pxweb_data_comments(px_data)
# px_data_comments_df <- as.data.frame(px_data_comments)
# 
# # Cite the data as 
# pxweb_cite(px_data)
# 
# saveRDS(px_data_frame, "verot.rds")

df <- readRDS("verot.rds")

maakunnat <- c("Uusimaa", "Southwest Finland", "Satakunta", "Kanta-Häme", "Pirkanmaa", "Päijät-Häme", "Kymenlaakso", "South Karelia", "South Savo", "North Savo", "North Karelia", "Central Finland", "South Ostrobothnia", "Ostrobothnia", "Central Ostrobothnia", "North Ostrobothnia", "Kainuu", "Lapland", "Åland")

df <- df |>
  filter(
    Region %in% maakunnat
  ) |> 
  transmute(
    Date = as.yearmon(`Tax Year`),
    Region = as.factor(Region),
    Variable,
    Value = Euro
  ) |> 
  filter(
    !is.na(Value)
  ) |> 
  arrange(Date, Region)


population <- read_delim("population.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


df <- inner_join(df, population, by=c("Region" = "Area"))
df <- df |> 
  mutate(
    PerCapita = Value / `2022`
  )

summary(df)

keskiarvot <- df |>
  group_by(Date) |> 
  transmute(
    Region,
    keskiarvo = mean(PerCapita)
  )





df <- left_join(df, keskiarvot, by = c("Date" = "Date", "Region" = "Region"))

p <- ggplot(df, aes(x = `Date`, y = `PerCapita`)) +
  geom_area(aes(fill = df$`Region`), show.legend = FALSE) +
  geom_line(aes(x = `Date`, y = `keskiarvo`), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "#333333") +
  facet_wrap(~Region) +
  bbc_style() +
  ylim(0, 3000) +
  theme(
    text = element_text(size = 40, family = "sans-serif"),
    panel.grid.major.y = element_line(color="#cbcbcb"),
    axis.text.x = element_text(angle = 45, size = 20, vjust = 0.99, hjust = 1),
    axis.text.y = element_text(size = 20),
    plot.caption = element_text(size = 30)
    ) +
  labs(
    title="Regional taxable Income (€ per capita) by Region in Finland",
       subtitle = "Taxable Income by Region from January 2020 to February 2023. The dashed line represents the country average."
       ) +
  scale_fill_hue()
p


ggsave("Day12-BBCPlot-4K.png", units = c("px"), width = 3840, height = 2160)


finalise_plot(plot_name = p,
              source_name = "Source: Incomes Register Finland | #30DayChartChallenge - BBC - Day 12 | DataViz: @janmoilanen_ek",
              save_filepath = "Day12-BBCPlot.png",
              width_pixels = 640,
              height_pixels = 550)







