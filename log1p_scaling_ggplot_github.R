# Load packages
library(tidyverse)
library(sf)
library(colorspace)

# Import example data
nc.sf <- st_read(
  dsn = system.file("gpkg/nc.gpkg", package = "sf"),
  quiet = TRUE)

# Lineair scaling
ggplot(
  data = nc.sf,
  mapping = aes(fill = SID74)) +
  geom_sf(size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays") +
  theme_minimal()

# Log1p scaling
ggplot(
  data = nc.sf,
  mapping = aes(fill = SID74)) +
  geom_sf(size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p") +
  theme_minimal()

# Log1p scaling with manual breaks
ggplot(
  data = nc.sf,
  mapping = aes(fill = SID74)) +
  geom_sf(size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 1, 2, 5, 10, 20, 50)) +
  theme_minimal()
