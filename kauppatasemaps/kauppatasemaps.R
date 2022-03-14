#install.packages(c("tmap", "geofi", "ggplot2", "tidyverse", "readr"))

library(tmap)
library(geofi)
library(ggplot2)
library(tidyverse)
library(readr)

# Tavarakauppa maakunnittain ---------------------------------------------------------

df.kauppa <- read_delim("tavarakauppa-maakunnittain.csv", 
                                                      delim = ";", escape_double = FALSE, na = "NA", 
                                                      trim_ws = TRUE)

finmap <- get_municipalities(year = 2020)
finmap <- finmap %>%
  transmute(
    geom = finmap$geom,
    vuosi = finmap$vuosi
  ) %>%
  count(maakunta_code)

p_regions <- ggplot(finmap %>% count(maakunta_code), aes(fill = maakunta_code)) + 
  geom_sf() + 
  
polygon1 <- get_municipalities(year = 2022)
df  <- get_municipalities(year = 2022)

plot(finmap, border = NA, col="black", main = NULL)

# load example datasets
df.mun <- get_municipalities(year = 2022) %>%
  filter(
    
  )

# draw polygons
tm_shape(df.mun) + tm_polygons()

p <- ggplot(df.mun, aes(fill = "black", color = "black")) +
  geom_sf()
p
plot(df.mun, border = NA, col="black", main = NULL)

# draw polygons with a specific color
tm_shape(df.mun) + tm_polygons("blue")
# draw polygons colored by a data variable
# the result is called a choropleth
tm_shape(df.mun) + tm_polygons("vuosi")
