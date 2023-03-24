library(ggplot2)
library(dplyr)
library(gganimate)
library(tmap)

library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

df.paatokset.kansalaisuus <- readRDS("Paatokset_tyo_kansalaisuus_aikasarja.rds")


urb_1970_2030 = urban_agglomerations |> 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

urb_anim = tm_shape(world)

tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)


mapdata <- map_data("world")


maat <- as.factor(df.paatokset.kansalaisuus$Kansalaisuus)

countries <- countrycode::countryname(maat)
codes <- countrycode::codelist |>
  select(
    cldr.name.fi,
    country.name.en,
    iso2c,
    iso3c
  )

j <- df.paatokset.kansalaisuus |>
  left_join(codes, by = c("Kansalaisuus" = "cldr.name.fi"))

x <- left_join(j, mapdata, by = c("country.name.en" = "region"))



plotdata <- x |> filter(x$Päätös == "Myönteinen" & Vuosi == 2015)


p <- ggplot(plotdata, aes(x = long, y = lat, group = group, text = row.names(~Kansalaisuus))) +
  geom_polygon(aes(fill = Määrä), colour = "grey80") +
  theme_minimal() +
  # theme(
  #   # axis.line = element_blank(),
  #   # panel.grid = element_blank(),
  #   # axis.title = element_blank(),
  #   # axis.text = element_blank(),
  #   plot.title = element_text(size = 15)
  # ) +
  facet_wrap(~Vuosi) +
  scale_fill_viridis_c() +
  coord_sf(crs = sf::st_crs(3112)) +
  xlab("") +
  ylab("") +
  labs(title = "Myönteiset työlupapäätökset maittain",
       subtitle = "Migrin Suomeen myöntämät työlupapäätökset vuodesta 2015 alkaen hakijan kansalaisuuden mukaan",
       caption = "Lähde: Migri")

p
