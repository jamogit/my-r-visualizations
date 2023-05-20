library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(pxweb)
library(stringr)
library(forcats)

font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


# PXWEB query 
pxweb_query_list <- 
  list("Kuukausi"=c("2015M01","2015M02","2015M03","2015M04","2015M05","2015M06","2015M07","2015M08","2015M09","2015M10","2015M11","2015M12","2016M01","2016M02","2016M03","2016M04","2016M05","2016M06","2016M07","2016M08","2016M09","2016M10","2016M11","2016M12","2017M01","2017M02","2017M03","2017M04","2017M05","2017M06","2017M07","2017M08","2017M09","2017M10","2017M11","2017M12","2018M01","2018M02","2018M03","2018M04","2018M05","2018M06","2018M07","2018M08","2018M09","2018M10","2018M11","2018M12","2019M01","2019M02","2019M03","2019M04","2019M05","2019M06","2019M07","2019M08","2019M09","2019M10","2019M11","2019M12","2020M01","2020M02","2020M03","2020M04","2020M05","2020M06","2020M07","2020M08","2020M09","2020M10","2020M11","2020M12","2021M01","2021M02","2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02"),
       "Hyödyke"=c("*"),
       "Tiedot"=c("vuosimuutos"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/en/StatFin/khi/statfin_khi_pxt_11xb.px",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
pxweb_cite(px_data)



df <- px_data_frame |> filter(Month == max(Month)) |> 
  mutate(
    Countti = nchar(gsub("[^.]", "", `Commodity`))
  ) |> 
  filter(
    Countti == 4 & Month == max(Month) & !is.na(`Annual change (%)`)
  )


df_bottom <- df |> arrange(`Annual change (%)`) |> mutate(grouppi = "Bottom") |> head(n = 20)
df_top <- df |> arrange(`Annual change (%)`) |> mutate(grouppi = "Top") |> tail(n = 20)


df <- rbind(df_top, df_bottom)




ggplot(df |> arrange(df$`Annual change (%)`)) +
  geom_col(aes(x = fct_reorder(df$Commodity, df$`Annual change (%)`), y = df$`Annual change (%)`,
               colour = grouppi,
               fill = grouppi,
               alpha = 0.3),
           show.legend = F) +
  coord_flip() +
  theme_void() +
  labs(
    title = "Top and bottom 20 commodities with highest/lowest price change in Finland 2023M02",
    # subtitle = "Histograms and densities of students' average Scores over Math, Reading and Science by gender in 2018. Females manage to score higher",
    x = "Commodity",
    y = "Annual price change, %",
    caption = "Data: Statistics Finland, CPI   |   #30DayChartChallenge - High/Low - Day 9   |   DataViz: @janmoilanen_ek",
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "#E9D8A6"),
    axis.text.x = element_text(angle = 50, size = 30),
    axis.title.x = element_text(size = 40),
    axis.text.y = element_text(size = 30, hjust = 0, margin = margin(0, 10, 0, 10, "pt"), colour = ifelse(
      df$grouppi == "Top", "#005F73", "#9B2226")
     ),
    panel.grid.minor = element_line(colour = "#E9D8A6"),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    # plot.background = element_rect(fill = "#001219"),
    plot.title = element_text(vjust = 1, margin = margin(1, 3, 1, 1, "in")),
    # plot.caption = element_text(size = 40),
  ) +
  scale_colour_manual(values = c("#9B2226", "#005F73")) +
  scale_fill_manual(values = c("#9B2226", "#005F73"))


ggsave("Day9-HighLowPlot-4K.png", units = c("px"), width = 3840, height = 2160)
