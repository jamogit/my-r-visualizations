library(tidyverse)
library(highcharter)


df <- read_tsv("DataLineageExampleData.txt", na = "NULL")



data <- df %>%
  select(
    SourceSystem,
    SourceTable,
    Concept,
    TargetEntity
  )

p <- hchart(data_to_sankey(data), "sankey")
p %>%
  hc_title(text="Apparatus Works Inc.") %>%
  hc_subtitle(text="Enterprise Data Warehouse - Data Lineage") %>%
  hc_caption(text="Data Lineage from source system tables to Data Vault target table") %>%
  hc_add_theme(hc_theme_darkunica() )


