library(ggplot2)
library(osmdata)
library(ggmap)
library(repr)
library(showtext)
library(sf)

font_add_google("Special Elite", family = "special")
showtext_auto()

available_features()
available_tags("railway")

maincolor = "#264653"
housecolor = "#264653"
railwaycolor = "#2A9D8F"


xmin = 4.2748
xmax = 4.4286
ymin = 50.8007
ymax = 50.8999


# hieman väljempi
# xmin = 4.2650
# xmax = 4.4380
# ymin = 50.7930
# ymax = 50.9070



m <- matrix(
  c(xmin, ymin, xmax, ymax),
  nrow = 2,
  dimnames = list(c("x", "y"), c("min", "max"))
)


Berlin_streets <- opq(m) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

Berlin_small_streets <- opq(m) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()


Berlin_rivers <- opq(m) %>%
  add_osm_features (features = c (
    "\"waterway\"=\"river\"",
    "\"waterway\"=\"canal\"",
    "\"waterway\"=\"dam\"",
    "\"waterway\"=\"stream\""
  )) %>%
  osmdata_sf()

Berlin_buildings <- opq(m) %>%
  add_osm_features (features = c (
    "\"building\"=\"yes\"",
    "\"building\"=\"apartments\"",
    "\"building\"=\"flat\"",
    "\"building\"=\"house\"",
    "\"building\"=\"residential\"",
    "\"building\"=\"office\"",
    "\"building\"=\"commercial\"",
    "\"building\"=\"industrial\"",
    "\"building\"=\"government\""
  )) %>%
  osmdata_sf()

Berlin_railways <- opq(m) %>%
  add_osm_features (features = c (
    "\"railway\"=\"rail\"",
    "\"railway\"=\"station\"",
    "\"railway\"=\"subway\"",
    "\"railway\"=\"tram\""
  )) %>%
  osmdata_sf()

# Berlin_map <- get_map(location = c(lon = xmin + (xmax - xmin)/2, lat = ymin + (ymax - ymin)/2), maptype = "hybrid", zoom = 11)
# ggmap(Berlin_map)



p <- ggplot(data = Berlin_rivers$osm_lines) +
  geom_sf(data = Berlin_streets$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 2,
          alpha = 1.0) +
  geom_sf(data = Berlin_small_streets$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 0.5,
          alpha = 0.9) +
  geom_sf(data = Berlin_rivers$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 6,
          alpha = 0.8) +
  geom_sf(data = Berlin_buildings$osm_polygons,
          inherit.aes = FALSE,
          color = housecolor,
          fill = "antiquewhite",
          size = 0.5,
          alpha = 0.7) +
  geom_sf(data = Berlin_railways$osm_lines,
          inherit.aes = FALSE,
          color = railwaycolor,
          size = 0.8,
          alpha = 1.0) +
  coord_sf(xlim = c(xmin , xmax ),
           ylim = c(ymin , ymax ),
           expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(colour = "antiquewhite", fill = "antiquewhite"),
    plot.margin = margin(3, 6, 6, 6, "cm"),
    panel.background = element_rect(colour = "antiquewhite", fill = "antiquewhite"),
    panel.border = element_rect(colour = "antiquewhite", fill = NA, size = 40),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "special", colour = maincolor, hjust = 0.5, size = 80, margin = margin(1, 1, 0, 1, "cm")),
    plot.subtitle = element_text(family = "special", colour = "#2A9D8F", hjust = 0.5, size = 40),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(label = "BRUXELLES", subtitle = "50°N  4°E" ) +
  xlab("Länge") +
  ylab("Breite")

p

#colours()
# 
ggsave("Bruxelles.png", width = 60, height = 60, limitsize = FALSE)
