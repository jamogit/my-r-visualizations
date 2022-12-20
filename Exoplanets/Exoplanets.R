library(exoplanets)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(cowplot)


# vignette("exoplanets")

# Tables and columns in NASA exoplanet archive
# https://exoplanetarchive.ipac.caltech.edu/docs/API_PS_columns.html

tableinfo %>%
  filter(table == "ps") %>%
  select(database_column_name, description)


discovery <- exoplanets(
  table = "ps",
  columns = c(
    "pl_name",
    "pl_orbper",
    "pl_massj",
    "discoverymethod",
    "hostname",
    "disc_year",
    "disc_locale",
    "disc_telescope",
    "releasedate",
    "pl_rade"
  )
)

n = discovery |> select(pl_name) |> distinct() |> count()



p1 <- discovery |>
  select(
    pl_name, disc_year
  ) |>
  distinct() |>
ggplot() +
  geom_bar(aes(x = disc_year)) +
  annotate ("text", x = 1995, y = 1250, label = paste("Total number exoplanets =", n)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(2,2,2,2, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(margin = margin(0, 0, 2, 0, "cm"))
  ) +
  labs(
    title = "Exoplanets discovered by Year",
    y = "Number of exoplanets",
    x = "Discovery year"
  )


p2 <- discovery |>
  mutate(
    hostname, pl_name, row_sum = 1
  ) |> group_by(hostname) |>  summarise()
  ggplot() +
  geom_bar(aes(x = hostname, y = row_sum)) +
  annotate ("text", x = 1995, y = 1250, label = paste("Total number exoplanets =", n)) +
  theme_minimal()
  # theme(
  #   panel.background = element_blank(),
  #   plot.margin = margin(2,2,2,2, "cm"),
  #   panel.grid.minor = element_blank(),
  #   plot.title = element_text(margin = margin(0, 0, 2, 0, "cm"))
  # ) +
  # labs(
  #   title = "Exoplanets discovered by Year",
  #   y = "Number of exoplanets",
  #   x = "Discovery year"
  # )

p2


ggdraw() +
  draw_plot(p, x = 0, y = 0, width = 0.5) +
  draw_plot(p2, x = 0.5, y = 0, width = 0.5)
