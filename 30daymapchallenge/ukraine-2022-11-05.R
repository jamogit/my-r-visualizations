library(ggplot2)
library(osmdata)
library(ggmap)
library(repr)
library(showtext)
library(maps)

font_add_google("Special Elite", family = "special")
showtext_auto()

available_features()
available_tags("border_type")

maincolor = "#264653"
housecolor = "#264653"
railwaycolor = "#2A9D8F"


# Ukraine
# Lat 48.3794
# Lon 31.1656

xmin = 21.577
xmax = 40.781
ymin = 43.676
ymax = 52.803



m <- matrix(
  c(xmin, ymin, xmax, ymax),
  nrow = 2,
  dimnames = list(c("x", "y"), c("min", "max"))
)


ukr_saunas <- opq(m) %>%
  add_osm_feature(key = "leisure",
                  value = c("sauna")) %>%
  osmdata_sf()

ukr_borders <- opq(m) %>%
  add_osm_features (features = c (
    "\"regions\"=\"Ukraine\""
  )) %>%
  osmdata_sf()


p <- ggplot() +
  borders(regions = "Ukraine", colour = "gray50", fill = "gray80", alpha = 0.9) +
  geom_sf(data = ukr_buildings$osm_points,
          inherit.aes = FALSE,
          color = maincolor,
          size = 2,
          alpha = 1.0) +
  geom_sf(data = ukr_borders$osm_lines,
          inherit.aes = FALSE,
          color = maincolor,
          size = 6,
          alpha = 0.8) +

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
    plot.title = element_text(family = "special", colour = maincolor, size = 300, margin = margin(1, 1, 0, 1, "cm")),
    plot.subtitle = element_text(family = "special", colour = "#2A9D8F", hjust = 0.5, size = 40),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(label = "Saunas in Ukraine") +
  xlab("Longitude") +
  ylab("Latitude")

p

# colours()
# 
# ggsave("ukr_map.png", width = 60, height = 50, limitsize = FALSE)
