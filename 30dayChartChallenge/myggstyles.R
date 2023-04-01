library(tidyverse)
library(readr)
library(viridis)
library(hrbrthemes)
library(ggthemes)
library(ggfx)
library(showtext)


font_add_google(name = "Big Shoulders Inline Text", regular.wt = 300, family = "sans-serif")
showtext_auto()

# Read data in ----
BKT_suomi <- read_delim("my-r-visualizations/30dayChartChallenge/BKT-suomi.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 2)


# Convert labels and reverse the sign of imports ----
df <- BKT_suomi |>
  transmute(
    Year = as.integer(str_sub(Vuosi, 1, 4)),
    `P3_S14_S15` = `Kaypiin hintoihin, miljoonaa euroa P3KS14_S15 S15 Yksityiset kulutusmenot (S14+S15), menona`,
    `P3_S13` = `Kaypiin hintoihin, miljoonaa euroa P3KS13 Julkiset kulutusmenot (S13), menona`,
    `P51` = `Kaypiin hintoihin, miljoonaa euroa P51K Kiintean paaoman bruttomuodostus, menona`,
    `P52_P53` = `Kaypiin hintoihin, miljoonaa euroa P52K Varaston muutos, menona` +
      `Kaypiin hintoihin, miljoonaa euroa P53K Arvoesineiden nettohankinta, menona`,
    `P6` = `Kaypiin hintoihin, miljoonaa euroa P6K Tavaroiden ja palvelujen vienti, menona`,
     `P7` = -1 * `Kaypiin hintoihin, miljoonaa euroa P7R Tavaroiden ja palvelujen tuonti, tulona`,
    `P6_P7` = `Kaypiin hintoihin, miljoonaa euroa P6K Tavaroiden ja palvelujen vienti, menona` -
      `Kaypiin hintoihin, miljoonaa euroa P7R Tavaroiden ja palvelujen tuonti, tulona`,
    `Error` = `Kaypiin hintoihin, miljoonaa euroa DEB1G  Tilastollinen ero`
  )

# Calculate GDP ----
df <- df |> 
  mutate(
    GDP = `P3_S14_S15` +
      `P3_S13` +
      `P51` +
      `P52_P53` +
      `P6` + `P7` +
      `Error`
  )

# Unpivot ----
df <- df |> pivot_longer(df, cols = !Year, names_to = "Component")

# Change label orders ----
df$Component <- factor(df$Component, levels = c("GDP", "P3_S14_S15", "P3_S13" ,"P51", "P6_P7", "P52_P53", "P6","P7", "Error"),
                       labels = c("Gross Domestic Product",
                                  "Private consumption (P.3 S.14+S.15)",
                                  "Public consumption (P.3 S.13)",
                                  "Investments (P.51)",
                                  "Gross Exports (P.6 - P.7)",
                                  "Changes in inventories (P.52 + P.53)",
                                  "Exports (P.6)",
                                  "Imports (P.7)",
                                  "Statistical discrepancy"))

plotdata = df |> filter( !(Component %in% c("Gross Domestic Product", "Exports (P.6)", "Imports (P.7)")) )
labels = df |> filter( Component == "Gross Domestic Product")

p <- ggplot(data = plotdata, aes(x = `Year`, y = `value`, fill = `Component`)) +
  geom_area(alpha = 0.6, size = 0.2, colour = "grey90") +
  # geom_point(data = labels, aes(x = labels$Year, y = labels$value)) +
  # geom_line(data = labels, aes(x = labels$Year, y = labels$value)) +
  annotate("text", x = labels$Year, y = labels$value, label = paste0(format(labels$value, big.mark = " "), ""),
           check_overlap = TRUE,
           hjust = 1.2, colour = "grey50", angle =-25, size = 8) +
  labs(
      title = "Components of Gross Domestic Product (GDP) in Finland between 1975-2022*, market prices",
      subtitle = "Gross Domestic Product (GDP) as presented in the European System of Accounts ESA2010.
GDP  = Private consumption (P.3 S.14+S.15) + Public consumption (P.3 S.13) + Investments (P.51) + Changes in inventories (P.52 + P.53) + Net exports (Exports P.6 - Imports P.7)",
      caption = "\nDatasource: Statistics Finland     |     #30DayChartChallenge - Day 1     |     DataViz: @janmoilanen_ek\n",
      alt = "Alternative description",
      x = "Year",
      y = "Gross Domestic Product, GDP, million euro"
    ) +
  theme_minimal() +
  # ggthemes::theme_economist_white() +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "black"),
    axis.line = element_line(colour = "#FFF3B0"),
    # axis.title = element_text(size = 12),
    # axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_line(colour = "#FFF3B0"),
    panel.grid = element_line(colour = "grey80"),
    plot.background = element_rect(fill = "grey90"),
    plot.margin = margin(1, 1, 0, 1, unit = "cm"),
    legend.position = "bottom",
    legend.title = element_blank()
    ) +
  scale_fill_economist() +
  scale_y_continuous(labels = scales::label_number_auto())

p

# 4K save ----
ggsave("GDPPlot-4K.png", units = c("px"), width = 3840, height = 2160)

