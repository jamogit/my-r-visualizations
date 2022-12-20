library(rayshader)
library(raster)
library(stars)
library(lidR)
library(terra)


# Read data ----

paljakka <- raster('..\\Paljakkamap\\R5123G.tif', nrows = 1000)

paljakka

r1 <- crop(paljakka, extent(544292,546476,7172234,7173884))


matr <- raster_to_matrix(r1)

matr %>%
  sphere_shade(sunangle = 45, texture = 'imhof1') %>%
  add_water(detect_water(matr), color = 'imhof1') %>%
  add_shadow(ray_shade(matr), 0.5) %>%
  add_shadow(ambient_shade(matr), 0) %>%
  plot_map()

matr %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(matr), color = "desert") %>%
  add_shadow(ray_shade(matr, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(matr), 0) %>%
  plot_3d(matr, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()


# Read LiDAR data ----

las <- readLAS('..\\Paljakkamap\\R5123G3.laz', filter = "-keep_first")
plot(las)

dtm <- rasterize_terrain(las, algorithm = tin(), pkg ="terra")

matr <- raster(dtm)

matr <- raster_to_matrix(matr)

map <- matr %>%
  sphere_shade(sunangle = 45, texture = 'imhof1') %>%
  add_water(detect_water(matr), color = 'imhof1') %>%
  add_shadow(ray_shade(matr), 0.5) %>%
  add_shadow(ambient_shade(matr), 0)

plot_map(map)


map <- matr %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(matr), color = "desert") %>%
  add_shadow(ray_shade(matr, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(matr), 0)

plot_3d(map, matr, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()