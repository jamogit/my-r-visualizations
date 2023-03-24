library(learningtower)
library(dplyr)
library(ggplot2)
library(ggridges)


df_all <- load_student("all")

df_all_fin <- df_all |>
  filter(df_all$country == "FIN")

summary(df_all_fin)





ggplot(df_all_fin, aes(x = read, y = year), group = gender) +
  geom_density_ridges(rel_min_height = 0.01,
                      scale = 3,
                      quantile_lines = TRUE,
                      quantiles = 2)


ggplot(df_all_fin, aes(x = math, fill = gender)) +
  geom_density() +
  facet_grid()
