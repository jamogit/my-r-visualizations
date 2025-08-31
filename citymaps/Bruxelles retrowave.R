library(ggplot2)
library(osmdata)
library(ggmap)
library(repr)
library(showtext)
library(sf)


# Specs ----

## Font ----
font_add_google("Orbitron", family = "retro")
showtext_auto()

## Colors ----
maincolor <- "#FF6EC7"     # kadut (pinkki)
housecolor <- "#00F0FF"    # rakennukset (syaani)
rivercolor <- "#8B00FF"  # joet (violetti)
railwaycolor <- "#FFEA00"  # raiteet (keltainen)
bgcolor <- "#0D0028"       # tausta

## Location ----
xmin = 4.2748
xmax = 4.4586
ymin = 50.8007
ymax = 50.8999


## Data ----
m <- matrix(
  c(xmin, ymin, xmax, ymax),
  nrow = 2,
  dimnames = list(c("x", "y"), c("min", "max"))
)

streets <- opq(m)  |>
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary"))  |>
  osmdata_sf()

small_streets <- opq(m) |>
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified", "service", "footway"))  |>
  osmdata_sf()

main_roads <- opq(m) %>%
  add_osm_feature(
    key = "highway", 
    value = c("motorway", "trunk", "primary")
  ) %>%
  osmdata_sf()


rivers <- opq(m) |>
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal", "dam", "stream")) |>
  osmdata_sf()


# buildings <- opq(m) |>
#   add_osm_feature(key = "building", 
#                   value = c("yes","apartments","flat","house","residential",
#                             "office","commercial","industrial","government")) |>
#   osmdata_sf()

railways <- opq(m) |>
  add_osm_feature(key = "railway", 
                  value = c("rail","station","subway","tram")) |>
  osmdata_sf()


# Plot ----

p <- ggplot() +
  # Kadut
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 4,
          alpha = 1.0) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 0.5,
          alpha = 0.8) +
  
  geom_sf(data = main_roads$osm_lines,
          inherit.aes = FALSE,
          color = "yellow",
          size = 16,
          alpha = 1.0) +
  # Joki viivoina
  geom_sf(data = rivers$osm_lines,
          inherit.aes = FALSE,
          color = rivercolor,
          size = 6,
          alpha = 1.0) +
  # Joki alueina (jos löytyy polygoneja)
  geom_sf(data = rivers$osm_polygons,
          inherit.aes = FALSE,
          fill = rivercolor,
          color = NA,
          alpha = 1.0) +
  # Rakennukset
  # geom_sf(data = buildings$osm_polygons,
  #         inherit.aes = FALSE,
  #         color = housecolor,
  #         fill = NA,
  #         size = 0.3,
  #         alpha = 0.8) +
  # Raiteet
  geom_sf(data = railways$osm_lines,
          inherit.aes = FALSE,
          color = housecolor,
          size = 0.8,
          alpha = 1.0) +
  
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax),
           expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(colour = bgcolor, fill = bgcolor),
    plot.margin = margin(5, 15, 15, 15, "cm"),
    panel.background = element_rect(colour = bgcolor, fill = bgcolor),
    panel.border = element_rect(colour = bgcolor, fill = NA, size = 40),
    panel.grid = element_blank(),
    plot.title = element_text(family = "retro", colour = "#FF6EC7", hjust = 0.5, size = 400, margin = margin(2, 2, 1, 2, "cm")),
    plot.subtitle = element_text(family = "retro", colour = "#00F0FF", hjust = 0.5, size = 250),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(label = "BRUXELLES", subtitle = "50°N  4°E")

p

ggsave("Bruxelles_retrowave2.png", width = 60, height = 60, limitsize = FALSE)



