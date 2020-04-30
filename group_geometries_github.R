# Load packages
library(sf)
library(dplyr)
library(rgdal)

# Read PC4 simple feature
pc4.sf <- st_read(dsn = ".../pc4 - poly/2004", layer = "pc4_2004", quiet = TRUE) %>%
	st_set_crs(28992) %>%
	rename(pc4 = PC4) %>%
	arrange(pc4)

# Are there any duplicated records? Which?
pc4.sf$pc4 %>% duplicated %>% any
pc4.sf$pc4 %>% duplicated %>% which

# Aggregate duplicated records by pc4 into multipolygons
pc4.sf.new <- pc4.sf %>%
  group_by(pc4) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_cast(to = "MULTIPOLYGON")

# Check
pc4.sf.new$pc4 %>% duplicated %>% any
dim(pc4.sf)
dim(pc4.sf.new)

#write output
st_write(pc4.sf.new, dsn = ".../pc4_2004.shp", layer = "pc4_2004", quiet = TRUE)

#dubbelcheck duplicates
# Read PC4 simple feature
pc4.sf.new2 <- st_read(dsn = "...", layer = "pc4_2004", quiet = TRUE) %>%
  st_set_crs(28992) %>%
  #rename(pc4 = PC4) %>%
  arrange(pc4)

# Are there any duplicated records? Which?
pc4.sf.new2$pc4 %>% duplicated %>% any
pc4.sf.new2$pc4 %>% duplicated %>% which
