#
# Init
#

# Load packages
library(sf)
library(dplyr)

# Source functions
source(".../st_make_hexgrid.R")

#
# Create hexagons
#

# Read CBS provincie
nl.sf <- st_read(dsn = ".../cbs_provincie_2017.geojson") %>% st_union

# Create hexagonal grids
hex.10km2.sf <- st_make_hexgrid(x = nl.sf, cellarea = 10e6) %>% mutate(hex = 1:nrow(.))
hex.20km2.sf <- st_make_hexgrid(x = nl.sf, cellarea = 20e6) %>% mutate(hex = 1:nrow(.))
hex.25km2.sf <- st_make_hexgrid(x = nl.sf, cellarea = 25e6) %>% mutate(hex = 1:nrow(.))
hex.50km2.sf <- st_make_hexgrid(x = nl.sf, cellarea = 50e6) %>% mutate(hex = 1:nrow(.))

# Plot
hex.10km2.sf %>% st_geometry %>% plot(lwd = 0.1, col = "white"); plot(nl.sf, add = TRUE)
hex.20km2.sf %>% st_geometry %>% plot(lwd = 0.1, col = "white"); plot(nl.sf, add = TRUE)
hex.25km2.sf %>% st_geometry %>% plot(lwd = 0.1, col = "white"); plot(nl.sf, add = TRUE)
hex.50km2.sf %>% st_geometry %>% plot(lwd = 0.1, col = "white"); plot(nl.sf, add = TRUE)

#
# Write
#

st_write(hex.10km2.sf, dsn = path.expand(".../hex_10km2.geojson"), delete_dsn = TRUE)
st_write(hex.20km2.sf, dsn = path.expand(".../hex_20km2.geojson"), delete_dsn = TRUE)
st_write(hex.25km2.sf, dsn = path.expand(".../hex_25km2.geojson"), delete_dsn = TRUE)
st_write(hex.50km2.sf, dsn = path.expand(".../hex_50km2.geojson"), delete_dsn = TRUE)
