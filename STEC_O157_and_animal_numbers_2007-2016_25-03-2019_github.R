#
# Init ----
#

# Load packages
 library(readxl)
 library(sf)
 library(dplyr)
 library(tidyr)
 library(lubridate)
 library(RColorBrewer)
 library(Matrix)
 library(RANN)
 library(tidyverse)
 library(colorspace)

#
# Read data ----
#

# Some conventions:
# .data = dataframe without spatial information
# .sf   = simple feature = dataframe with spatial information
# .year = list of dataframes of sf's for each year
# .pc4  = pc4 polygons
# .pc6  = pc6 points
# .hex  = hex polygons
# .xy   = xy coordinates

# Set years
years <- 2007:2016
n.years <- length(years)

# Read pc4 population numbers for each year
pop.pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pop.pc4.year.data[[i]] <- read.delim(file = paste0(".../BevolkingPerPostcode_1januari", years[i], ".txt")) %>%
    # Select relevant columns
    select(pc4, mannen_0tot5:mannen_95tot100, vrouwen_0tot5:vrouwen_95tot100) %>%
    # Reshape population numbers into long format
    gather(key = "sex_agecat", value = "population", -pc4) %>%
    # Add year
    mutate(year = years[i])
}

# Read pc4 polygons for each year
pc4.year.sf <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pc4.year.sf[[i]] <- st_read(
    dsn = paste0(".../pc4 - poly/", years[i]),
    layer = paste0("pc4_", years[i]),
    type = 6,
    quiet = TRUE) %>%
    # Add year
    mutate(year = years[i]) %>%
    # Set CRS to 29882 (ignore warning)
    st_set_crs(28992)
}


# Rename pc4 workaround
pc4.year.sf <- lapply(X = pc4.year.sf, FUN = function(data) {
  # Rename the different original pc4 names (pc4, PC4, postcode, etc.) to pc4
  # Be sure pc4 is in the first column
  names(data)[1] <- "pc4"
  #rename(pc4 = PC4) %>%
  data %>%
  arrange(pc4)
})


# Check dubbele postcodes
sapply(X = pc4.year.sf, FUN = function(data) data$pc4 %>% duplicated %>% sum)
# Are there any duplicated records? Which?
pc4.year.sf[[i]]$pc4 %>% duplicated %>% any
pc4.year.sf$pc4 %>% duplicated %>% which

# Read pc6 points and population (2016)
pop.pc6.sf <- read.delim(file = ".../pc6_2016.txt") %>%
  # Rename inwoners -> population
  rename(population = inwoners) %>%
  # Drop pc6 with 0 inhabitants
  filter(population > 0) %>%
  # Convert to sf object
  st_as_sf(coords = c("x", "y")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

# Read hexagonal reference map

# Area is 10 km^2
#hex.sf <- st_read(dsn = ".../hex_10km2.geojson", quiet = TRUE) %>%
  # Set CRS to 29882 (ignore warning)
#  st_set_crs(28992)

# Read hexagonal reference map
# Area is 20 km^2
#hex.sf <- st_read(dsn = ".../hex_20km2.geojson", quiet = TRUE) %>%
  # Set CRS to 29882 (ignore warning)
#  st_set_crs(28992)

# Read hexagonal reference map
# Area is 25 km^2
#hex.sf <- st_read(dsn = ".../hex_25km2.geojson", quiet = TRUE) %>%
  # Set CRS to 29882 (ignore warning)
#  st_set_crs(28992)

# Read hexagonal reference map
# Area is 50 km^2
#hex.sf <- st_read(dsn = ".../hex_50km2.geojson", quiet = TRUE) %>%
  # Set CRS to 29882 (ignore warning)
#  st_set_crs(28992)

# Read hexagonal reference map
# Area is 90 km^2
hex.sf <- st_read(dsn = ".../hex_90km2.geojson", quiet = TRUE) %>%
# Set CRS to 29882 (ignore warning)
 st_set_crs(28992)

# Write hexagon shapefile for article
st_write(hex.sf, dsn = "hex_90.shp", layer = "hex_90.shp", driver = "ESRI Shapefile")

# Read pc4 STEC data
stec.pc4.data <- read_excel(path = ".../STEC_cases.xlsx") %>%
  # Convert to dataframe (was tibble)
  as.data.frame %>%
    # Filter not buitenland = 1, not missing postcode, 1011 <= postcode <= 9999, Geslacht not 'Onb'
  filter(buitenland != 1 & !is.na(pc4) & !is.na(pc6) & pc4 >= 1011 & pc4 <= 9999, Geslacht != "Onb", uitslag == "O157") %>%
  # Rename columns
  rename(
    onset.date = EZD,
    report.date = meldingsdatum,
    #pc4 = postcode,
    age = Age,
    sex = Geslacht,
    subtype = uitslag) %>%
    # Select relevant columns
  select(c(onset.date, report.date, pc4, age, sex, subtype)) %>%
  # Sort by pc4
  arrange(pc4)

# Read point animal data (2012)
animal.xy.sf <- read_excel(path = ".../LBT_bedrijf_2012.xlsx") %>%
  # Convert to dataframe
  as.data.frame %>%
  # Select relevant columns
  select(c(x_coor, y_coor, Varkens_totaal, Pluimvee_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Herkauwers_totaal, Kleine_Herkauwers_totaal,
           Rund_Melk_totaal, Rund_Vlees_totaal)) %>%
  # Select records that have at least observation (= remove records with all NA's)
  filter_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Herkauwers_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal),
    .vars_predicate = any_vars(!is.na(.))) %>%
  # Convert to simple feature
  st_as_sf(coords = c("x_coor", "y_coor")) %>%
  # Set CRS to 28992 (RD_NEW). Ignore warning
  st_set_crs(28992)

#
# Summarize population and stec cases over predefined age categories ----
# Do this for each pc4 and for each year
#

# Define age categories as multiples of 5
# Multiples of 5 because of the 5y age categories in pop.pc4.year.data
# Intervals are open one the right
# Example: breaks = 0, 15, 30 -> intervals = [0, 15), [15, 30)
age.breaks <- c(0, 5, 10, 50, Inf)

# In pop.pc4.year.data, add age categories and summarize population numbers
# Use lapply because this is a list of dataframes
pop.pc4.year.data <- pop.pc4.year.data %>%
  lapply(FUN = function(data) {
    data %>%
      # First Separate sex_agecat into sex and agecat. E.g. mannen_0tot5 -> mannen, 0tot5
      separate(col = sex_agecat, into = c("sex", "agecat"), sep = "_") %>%
      # Then separate agecat into agelwr and ageupr. Convert to integer. E.g. 0tot5 -> 0, 5
      separate(col = agecat, into = c("agelwr", "ageupr"), sep = "tot", convert = TRUE) %>%
      # Add age categories by cutting mean(c(agelwr, ageupr - 1)). E.g. 0, 5 -> 2 -> [0, 5)
      # Intervals are open on the right
      mutate(agecat = cut((agelwr + (ageupr - 1))/2, breaks = age.breaks, right = FALSE, include.lowest = TRUE)) %>%
      # Relabel mannen -> male and vrouwen -> female
      mutate(sex = recode_factor(sex, mannen = "male", vrouwen = "female")) %>%
      # Group by year, pc4, agecat, sex
      # Summarize population numbers
      group_by(year, pc4, agecat, sex) %>%
      summarize(population = sum(population)) %>%
      # Convert to data.frame (was tibble)
      as.data.frame
  })

# In stec.pc4.data add age categories and summarize case numbers
# Split stec.pc4.data by year
stec.pc4.year.data <- stec.pc4.data %>%
  # First some modifications
  mutate(
    # Relabel Man -> male and Vrouw -> female
    sex = recode_factor(sex, Man = "male", Vrouw = "female"),
    # Convert onset.date and report.date to Date (was POSIXct)
    onset.date  = as.Date(onset.date),
    report.date = as.Date(report.date),
    # If onset.date is missing, replace by report.date
    onset.date = is.na(onset.date) %>%
      ifelse(yes = report.date, no = onset.date) %>%
      as.Date(origin = "1970-01-01"),
    # Add year (based on onset.date)
    year = year(onset.date),
    # Add season (summer = May - Oct, winter = Nov - Apr)
    #season = as.factor(season),
    season = month(onset.date) %in% 5:10 %>%
      factor(levels = c(FALSE, TRUE), labels = c("winter", "summer")),
    # Add age categories
    agecat = as.numeric(age) %>% cut(breaks = age.breaks, right = FALSE, include.lowest = TRUE)) %>%
  # Filter years 2007:2016
  filter(year %in% 2007:2016) %>%
  # Group by year, pc4, agecat, sex, season
  # Summarize cases
  group_by(year, pc4, agecat, sex, season) %>%
  summarize(cases = n()) %>%
  # Convert to dataframe (was tibble)
  as.data.frame %>%
  # Split by year
  split(f = .$year)

# Join pop.pc4.year.data and stec.pc4.data by year
pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  # First complete records to match pc4 levels in pc4.year.sf
  # year, agecat and sex are complete in pop.pc4.year.data
  # season is complete in stec.pc4.year.data
  # pc4 is complete in pc4.year.sf
  tmp <- expand.grid(
    year   = pop.pc4.year.data[[i]]$year %>% unique,
    pc4    = pc4.year.sf[[i]]$pc4,
    agecat = pop.pc4.year.data[[i]]$agecat %>% levels,
    sex    = pop.pc4.year.data[[i]]$sex %>% levels,
    season = stec.pc4.year.data[[i]]$season %>% levels)

  # Left join these complete combinations with pop.pc4.year.data and stec.pc4.data
  pop.pc4.year.data[[i]] <- left_join(tmp, pop.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      population = is.na(population) %>% ifelse(yes = 0, no = population),
      # Divide population by 4 because of season (4x 3 months)
      #population = population / 4)
      # Divide population by 2 because of season (2x 6 months)
      population = population / 2)
  stec.pc4.year.data[[i]] <- left_join(tmp, stec.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      cases = is.na(cases) %>% ifelse(yes = 0, no = cases))

  # Join pop.pc4.year.data with stec.pc4.data into pc4.year.data
  pc4.year.data[[i]] <- full_join(pop.pc4.year.data[[i]], stec.pc4.year.data[[i]])
  }

#
# Population weighted interpolation of pc4.year.data to hexagonal reference map ----
#

# Make intersection between hexagons and pc6 (as sparse matrix)
# (same for each year, therefore outside the loop)
hex.pc6 <- st_intersects(hex.sf, pop.pc6.sf)
hex.pc6 <- sparseMatrix(
  i = hex.pc6 %>% seq_along %>% rep(times = hex.pc6 %>% sapply(FUN = length)),
  j = hex.pc6 %>% unlist,
  dims = c(nrow(hex.sf), nrow(pop.pc6.sf)),
  x = 1)

# Some pc6's have population or cases (after distribution) but these are not aggregated because there is no hex associated with them
# Which pc6's have no hex?
jx <- which(colSums(hex.pc6) == 0)
# Find nearest hex using fast nearest neighbour search
ix <- nn2(
  data  = hex.sf %>% st_centroid %>% st_coordinates,
  query = pop.pc6.sf[jx, ] %>% st_coordinates,
  k = 1) %>% .$nn.idx %>% as.vector
# Assign nearest hex to these pc6's
hex.pc6 <- hex.pc6 + sparseMatrix(i = ix, j = jx, dims = c(nrow(hex.sf), nrow(pop.pc6.sf)), x = 1)

# For each year
hex.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {

  # Print progress
  print(years[i])

  # Make intersection between pc6 and pc4 (as sparse matrix)
  # (differs between years, therefore within the loop)
  pc6.pc4 <- st_intersects(pop.pc6.sf, pc4.year.sf[[i]])
  pc6.pc4 <- sparseMatrix(
    i = pc6.pc4 %>% seq_along %>% rep(times = pc6.pc4 %>% sapply(FUN = length)),
    j = pc6.pc4 %>% unlist,
    dims = c(nrow(pop.pc6.sf), nrow(pc4.year.sf[[i]])),
    x = 1)

  # Some pc4's have population or cases, but these are not distributed because there is no pc6 associated with them
  # # Which pc4's have no pc6?
  jx <- which(colSums(pc6.pc4) == 0)
  # Find nearest pc6 using fast nearest neighbour search
  ix <- nn2(
    data  = pop.pc6.sf %>% st_coordinates,
    query = pc4.year.sf[[i]][jx, ] %>% st_centroid %>% st_coordinates,
    k = 1) %>% .$nn.idx %>% as.vector
  # Assign nearest pc6 to these pc4's
  pc6.pc4 <- pc6.pc4 + sparseMatrix(i = ix, j = jx, dims = c(nrow(pop.pc6.sf), nrow(pc4.year.sf[[i]])), x = 1)

  # Add pc6 population numbers to columns of pc6.pc4
  pc6.pc4 <- pc6.pc4 * pop.pc6.sf$population

  # Calculate pc6 population fractions for each pc4 area by dividing each column by its column total
  # Use a trick for sparse matrices:
  # see https://stackoverflow.com/questions/39284774/column-rescaling-for-a-very-large-sparse-matrix-in-r
  pc6.pc4@x <- pc6.pc4@x / rep.int(colSums(pc6.pc4), diff(pc6.pc4@p))

  # R is the redistribution matrix pc4 -> pc6 -> hex
  # All we have to do is to multiply pc4 population or cases with this matrix to get the numbers in the hexagons
  R <- hex.pc6 %*% pc6.pc4

  # Because we have multiple strata, split pc4.year.data by agecat, sex, season
  stratum.pc4.year.data <- pc4.year.data[[i]] %>% split(f = list(.$agecat, .$sex, .$season))

  # For each stratum, redistribute pc4 population and cases to the hexagons
  stratum.hex.year.data <- stratum.pc4.year.data %>% lapply(FUN = function(data) {
    data.frame(
      # For each hex, these are constant:
      year = data$year %>% unique,
      hex = hex.sf %>% nrow %>% seq_len,
      agecat = data$agecat %>% unique,
      sex = data$sex %>% unique,
      season = data$season %>% unique,
      # But these must be redistributed:
      population = R %*% data$population %>% as.vector,
      cases = R %*% data$cases %>% as.vector)
  })

  # rbind stratum.hex.year.data back into one dataframe
  hex.year.data[[i]] <- stratum.hex.year.data %>% do.call(what = "rbind")
}

# Finally rbind hex.year.data into one dataframe
hex.data <- hex.year.data %>% do.call(what = "rbind")

#----------------------------------------------------------------------------------------------------------------------------------------------------
 # dierdichtheid in aantal/km2 (per hexagon) - use the below code if you want to use animal density instead of the population weighted animal number
#----------------------------------------------------------------------------------------------------------------------------------------------------

# #Join animal data with hexagons
# animal.pc6.buffer <- st_join(hex.sf, animal.xy.sf, left = TRUE) %>%
# # Replace NAs with 0
#   mutate_at(
#     .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal),
#     .funs = funs(replace(., list = is.na(.), values = 0)))
# head(animal.pc6.buffer)
#
# # Sum animals within each hexagon
# # (use aggregate, is much faster than dplyr's group_by/summarize)
# pc6.animal.data <- aggregate(
#   formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal) ~ hex,
#   FUN = sum,
#   data = animal.pc6.buffer)
# head(pc6.animal.data)
#
# #Join animal data with hexagons
# animal.pc6.buffer <- st_join(hex.sf, animal.xy.sf, left = TRUE) %>%
#   # Replace NAs with 0
#   mutate_at(
#     .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Kleine_Herkauwers_totaal),
#     .funs = funs(replace(., list = is.na(.), values = 0)))
# head(animal.pc6.buffer)
#
# # Sum animals within each hexagon
# # (use aggregate, is much faster than dplyr's group_by/summarize)
# pc6.animal.data <- aggregate(
#  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Kleine_Herkauwers_totaal) ~ hex,
#   FUN = sum,
#   data = animal.pc6.buffer)
# head(pc6.animal.data)
#
# # combine animal data with hex geometry to create .sf
# animal.hex.sf <- merge(
#   hex.sf,
#   pc6.animal.data)
#
# # Add surface area per hexagon and change unit to km2 instead of m2.
#  animal.hex.sf$surface <- set_units(st_area(animal.hex.sf$geometry), km^2)
#
# # Calculate animal density per hexagon
# animal.hex.sf <- animal.hex.sf %>%
#   mutate(
#     Pluimvee_density_km2 = Pluimvee_totaal/surface %>% as.numeric,
#     Varkens_density_km2  = Varkens_totaal/surface %>% as.numeric,
#     Rundvee_density_km2  = Rundvee_totaal /surface %>% as.numeric,
#     #Geiten_density_km2   = Geiten_totaal  /surface %>% as.numeric,
#     #Schapen_density_km2  = Schapen_totaal /surface %>% as.numeric,
#     Rundvee_density_km2 = Rundvee_totaal/surface %>% as.numeric,
#     Kleine_Herkauwers_density_km2 = Kleine_Herkauwers_totaal/surface %>% as.numeric)
#
# head(animal.hex.sf)

#------------------------------------------------------------------------------------------

#
# Population weighted number of animals within 1 km for each hex ----
#
hex.sf %>% st_area %>% as.numeric

# Create 1 km buffer around each pc6 point
pop.pc6.buffer.sf <- pop.pc6.sf %>%
  st_buffer(dist = 1000, nQuadSegs = 5)

# Spatial left join pop.pc6.buffer.sf with animal.xy.sf
  animal.pc6.buffer <- st_join(pop.pc6.buffer.sf, animal.xy.sf, left = TRUE) %>%
#    Replace NAs with 0
  mutate_at(
  .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Herkauwers_totaal, Kleine_Herkauwers_totaal,
               Rund_Melk_totaal, Rund_Vlees_totaal),
    .funs = funs(replace(., list = is.na(.), values = 0)))

# Sum animals within each pc6 1 km buffer
# (use aggregate, is much faster than dplyr's group_by/summarize)
pc6.animal.data <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Herkauwers_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer)

# Construct population weight matrix
# Transpose hex.pc6 for easier calculations
pc6.hex <- t(hex.pc6)
# Add pc6 population numbers to columns of pc6.hex
pc6.hex <- pc6.hex * pop.pc6.sf$population
# Calculate pc6 population fractions for each hex by dividing each column by its hex total (use trick)
pc6.hex@x <- pc6.hex@x / rep.int(colSums(pc6.hex), diff(pc6.hex@p))

# Calculate weighted number of animals within 1 km for each hex
animal.hex.sf <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data %>% select(-pc6) %>% as.matrix)) %>% as.matrix)

#
# Exploratory analysis ----
#

 # Start with plotting the data
#
 # Aggegrate population and cases over all years
 # Calculate raw incidence
 hex.agg.sf <- hex.data %>%
   # Summarize population and cases
   group_by(hex) %>%
   summarize(population = sum(population), cases = sum(cases)) %>%
   # Calculate raw incidence per 100,000
   # Replace NA's by 0
   mutate(incidence = (cases/population*1e5) %>% replace(., list = is.na(.), values = 0)) %>%
   # Join with hex.sf
   full_join(hex.sf) %>%
   # Convert to sf object
   st_as_sf

# Export to shapefile
st_write(hex.agg.sf, ".../hexaggsf.shp")

# Export animal.hex.sf as shapefile, to be able to use it in ArcGIS as well
st_write(animal.hex.sf, ".../animalhexsf.shp")

#
# # Open pdf
 pdf(file = ".../... .pdf", onefile = TRUE, width = 21/2.54, height = 29.7/2.54, paper = "a4")
#
# # Population
 plot(hex.agg.sf[, "population"],
   breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "A)")

 # Raw incidence
 plot(hex.agg.sf[, "incidence"],
   breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "STEC incidence per 100,000 2007 - 2016")

 # Poultry density
# plot(animal.hex.sf[, "Pluimvee_density_km2"],
#   breaks = "fisher", nbreaks = 50,
#   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
#   key.pos = 1,
#   lwd = 0.1,
#   main = "Pluimvee dichtheid (aantal/km2) 2012")

 # log1p scaling - Poultry total
 ggplot(
   data = animal.hex.sf,
   mapping = aes(fill = Pluimvee_totaal)) +
   geom_sf(size = 0.1) +
   scale_fill_continuous_sequential(
     palette = "Grays",
     trans = "log1p",
     breaks = c(0, 10, 100, 1000, 10000, 100000),
     labels = scales::comma_format()) +
   theme(
     panel.background = element_blank(),
     legend.title = element_blank()) +
   coord_sf(datum = NA) +
   ggtitle("Poultry total 2012")

# # Cattle density
# plot(animal.hex.sf[, "Rundvee_density_km2"],
#   breaks = "fisher", nbreaks = 50,
#   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
#   key.pos = 1,
#   lwd = 0.1,
#   main = "Rundvee dichtheid (aantal/km2) 2012")

 # log1p scaling - Cattle total
 ggplot(
   data = animal.hex.sf,
   mapping = aes(fill = Rundvee_totaal)) +
   geom_sf(size = 0.1) +
   scale_fill_continuous_sequential(
     palette = "Grays",
     trans = "log1p",
     breaks = c(0, 10, 100, 1000, 4000),
     labels = scales::comma_format()) +
   theme(
     panel.background = element_blank(),
     legend.title = element_blank()) +
   coord_sf(datum = NA) +
   ggtitle("Cattle total 2012")

 # Melk en kalfkoeien
 plot(animal.hex.sf[, "Rund_Melk_totaal"],
  breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "Dairy cows and calves 2012")

 # log1p scaling - Milk cows and calves
 ggplot(
   data = animal.hex.sf,
   mapping = aes(fill = Rund_Melk_totaal)) +
   geom_sf(size = 0.1) +
   scale_fill_continuous_sequential(
     palette = "Grays",
     trans = "log1p",
     breaks = c(0, 10, 100, 700),
     labels = scales::comma_format()) +
   theme(
     panel.background = element_blank(),
     legend.title = element_blank()) +
   coord_sf(datum = NA) +
   ggtitle("Dairy cows and calves 2012")

 # Meat calves
 plot(animal.hex.sf[, "Rund_Vlees_totaal"],
   breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "Veal calves 2012")

 # # Pigs density
# plot(animal.hex.sf[, "Varkens_density_km2"],
#   breaks = "fisher", nbreaks = 50,
#   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
#   key.pos = 1,
#   lwd = 0.1,
#   main = "Varkens dichtheid (aantal/km2) 2012")

 # log1p scaling - Pigs total
 ggplot(
   data = animal.hex.sf,
   mapping = aes(fill = Varkens_totaal)) +
   geom_sf(size = 0.1) +
   scale_fill_continuous_sequential(
     palette = "Grays",
     trans = "log1p",
     breaks = c(0, 10, 100, 1000, 10000, 40000),
     labels = scales::comma_format()) +
   theme(
     panel.background = element_blank(),
     legend.title = element_blank()) +
   coord_sf(datum = NA) +
   ggtitle("Pigs total 2012")

 # Ruminants total
 plot(animal.hex.sf[, "Herkauwers_totaal"],
      breaks = "fisher", nbreaks = 50,
      pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
      key.pos = 1,
      lwd = 0.1,
      main = "Herkauwers totaal 2012")

# # Goat density
# plot(animal.hex.sf[, "Geiten_density_km2"],
#   breaks = "fisher", nbreaks = 50,
#   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
#   key.pos = 1,
#   lwd = 0.1,
#   main = "Geiten dichtheid (aantal/km2) 2012")

 # Goats total
 plot(animal.hex.sf[, "Geiten_totaal"],
   breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "Geiten totaal 2012")

 # Sheep density
# plot(animal.hex.sf[, "Schapen_density_km2"],
#   breaks = "fisher", nbreaks = 50,
#   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
#   key.pos = 1,
#   lwd = 0.1,
#   main = "Schapen dichtheid (aantal/km) 2012")

 # Sheep total
 plot(animal.hex.sf[, "Schapen_totaal"],
   breaks = "fisher", nbreaks = 50,
   pal = colorRampPalette(brewer.pal(name = "YlOrRd", n = 9)),
   key.pos = 1,
   lwd = 0.1,
   main = "Schapen totaal 2012")

 # Small ruminants total
 plot(animal.hex.sf[, "Kleine_Herkauwers_totaal"],
      breaks = "fisher", nbreaks = 50,
      pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
      key.pos = 1,
      lwd = 0.1,
      main = "Small ruminants total 2012")

 # # Small ruminants total - density
 # plot(animal.hex.sf[, "Kleine_Herkauwers_density_km2"],
 #   breaks = "fisher", nbreaks = 50,
 #   pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
 #   key.pos = 1,
 #   lwd = 0.1,
 #   main = "Small ruminants density 2012")

  # log1p scaling - Small ruminants total
 ggplot(
   data = animal.hex.sf,
   mapping = aes(fill = Kleine_Herkauwers_totaal)) +
   geom_sf(size = 0.1) +
   scale_fill_continuous_sequential(
     palette = "Grays",
     trans = "log1p",
     breaks = c(0, 10, 100, 500, 2000),
     labels = scales::comma_format()) +
   theme(
     panel.background = element_blank(),
     legend.title = element_blank()) +
   coord_sf(datum = NA) +
   ggtitle("Small ruminants total 2012")

 # Close pdf
 dev.off()

#
# Spatial analysis ----
#
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#install INLA op server (werkt mogelijk alleen in de console):
INLA:::inla.dynload.workaround()

# Load packages
library(INLA)

# Source sf2nb function (spatial neighbours function)
source(file = ".../sf2nb.R")

# Get spatial neighbours of hex.sf
hex.nb <- sf2nb(hex.sf)

# Plot spatial neighbours
graphics.off()
plot(hex.nb, coords = hex.sf %>% st_centroid %>% st_coordinates, pch = ".", lwd = 0.1)

# Aggregate hex.data over years
hex.agg.data <- hex.data %>%
  group_by(hex, agecat, sex, season) %>%
  summarize(
    population = sum(population),
    cases = sum(cases)) %>%
  # Some modifications
  mutate(
    cases = ifelse(population == 0, yes = NA, no = cases),
    population = ifelse(population == 0, yes = 1, no = population),
    hex.car = hex,
    hex.iid = hex)

#-------------------------------------------------------------------------------------------------------------------
# Input article

# summary case dataset
summary(as.factor(stec.pc4.data$age))
summary(as.factor(stec.pc4.data$sex))
summary(as.factor(stec.pc4.data$subtype))
#summary(as.factor(stec.pc4.data$onset.date))

# summary cases per year
sum(stec.pc4.year.data$`2007`$cases)
sum(stec.pc4.year.data$`2008`$cases)
sum(stec.pc4.year.data$`2009`$cases)
sum(stec.pc4.year.data$`2010`$cases)
sum(stec.pc4.year.data$`2011`$cases)
sum(stec.pc4.year.data$`2012`$cases)
sum(stec.pc4.year.data$`2013`$cases)
sum(stec.pc4.year.data$`2014`$cases)
sum(stec.pc4.year.data$`2015`$cases)
sum(stec.pc4.year.data$`2016`$cases)

#Total number of cases included in analysis (excluding the cases with onset.date = NA)
sum((sum(stec.pc4.year.data$`2007`$cases)) + (sum(stec.pc4.year.data$`2008`$cases)) + (sum(stec.pc4.year.data$`2009`$cases)) +
      (sum(stec.pc4.year.data$`2010`$cases)) + (sum(stec.pc4.year.data$`2011`$cases)) + (sum(stec.pc4.year.data$`2012`$cases)) +
      (sum(stec.pc4.year.data$`2013`$cases)) + (sum(stec.pc4.year.data$`2014`$cases)) + (sum(stec.pc4.year.data$`2015`$cases)) +
      (sum(stec.pc4.year.data$`2016`$cases)))

#summary season data stec.pc4.data
stec.pc4.data.season <- stec.pc4.data %>% mutate(
  # Add season (summer = May - Oct, winter = Nov - Apr)
season = month(onset.date) %in% 5:10 %>%
  factor(levels = c(FALSE, TRUE), labels = c("winter", "summer")))

summary(stec.pc4.data.season$season)

#summarize case data after joining to population data before interpolation
#check of we nog steeds met 439 cases de analyse in gaan, dit klopt :D
sum(pc4.year.data[[1]]$cases)
sum(pc4.year.data[[2]]$cases)
sum(pc4.year.data[[3]]$cases)
sum(pc4.year.data[[4]]$cases)
sum(pc4.year.data[[5]]$cases)
sum(pc4.year.data[[6]]$cases)
sum(pc4.year.data[[7]]$cases)
sum(pc4.year.data[[8]]$cases)
sum(pc4.year.data[[9]]$cases)
sum(pc4.year.data[[10]]$cases)

sum(sum(pc4.year.data[[1]]$cases), sum(pc4.year.data[[2]]$cases), sum(pc4.year.data[[3]]$cases), sum(pc4.year.data[[4]]$cases),
    sum(pc4.year.data[[5]]$cases), sum(pc4.year.data[[6]]$cases), sum(pc4.year.data[[7]]$cases), sum(pc4.year.data[[8]]$cases),
    sum(pc4.year.data[[9]]$cases), sum(pc4.year.data[[10]]$cases))

#summarize animal data
#summarize total number of animals per animal group
sum(animal.xy.sf$Varkens_totaal, na.rm = TRUE)
sum(animal.xy.sf$Pluimvee_totaal, na.rm = TRUE)
sum(animal.xy.sf$Herkauwers_totaal, na.rm = TRUE)
sum(animal.xy.sf$Rundvee_totaal, na.rm = TRUE)
sum(animal.xy.sf$Geiten_totaal, na.rm = TRUE)
sum(animal.xy.sf$Schapen_totaal, na.rm = TRUE)
sum(animal.xy.sf$Kleine_Herkauwers_totaal, na.rm = TRUE)
sum(animal.xy.sf$Rund_Melk_totaal, na.rm = TRUE)
sum(animal.xy.sf$Rund_Vlees_totaal, na.rm = TRUE)

#summarize total number of farms per animal group
sum(animal.xy.sf$Varkens_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Pluimvee_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Herkauwers_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Rundvee_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Geiten_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Schapen_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Kleine_Herkauwers_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Rund_Melk_totaal > 0, na.rm=TRUE)
sum(animal.xy.sf$Rund_Vlees_totaal > 0, na.rm=TRUE)

# Check x>100 -> animal.hex.sf
# Small ruminants
# Mean
mean(animal.hex.sf$Kleine_Herkauwers_totaal)
# Median
median(animal.hex.sf$Kleine_Herkauwers_totaal)
# Subset x > 100
xabove100 <- subset(animal.hex.sf, Kleine_Herkauwers_totaal > 100)
# Subset x < 100
xbelow100 <- subset(animal.hex.sf, Kleine_Herkauwers_totaal <= 100)

# Cattle
# Mean
mean(animal.hex.sf$Rundvee_totaal)
# Median
median(animal.hex.sf$Rundvee_totaal)
# Subset x > 100
xabove100 <- subset(animal.hex.sf, Rundvee_totaal > 100)
# Subset x < 100
xbelow100 <- subset(animal.hex.sf, Rundvee_totaal <= 100)

# Poultry
# Mean
mean(animal.hex.sf$Pluimvee_totaal)
# Median
median(animal.hex.sf$Pluimvee_totaal)
# Subset x > 100
xabove100 <- subset(animal.hex.sf, Pluimvee_totaal > 100)
# Subset x < 100
xbelow100 <- subset(animal.hex.sf, Pluimvee_totaal <= 100)

# Pigs
# Mean
mean(animal.hex.sf$Varkens_totaal)
# Median
median(animal.hex.sf$Varkens_totaal)
# Subset x > 100
xabove100 <- subset(animal.hex.sf, Varkens_totaal > 100)
# Subset x < 100
xbelow100 <- subset(animal.hex.sf, Varkens_totaal <= 100)

#---------------------------------------------------------------------------------------------------------------------------

#
# Model without animals ----
#

#including relevel function
hex.agg.data$agecat<-relevel(hex.agg.data$agecat, ref = "[50,Inf]")

# Model without animals
mod <- inla(
  formula = cases ~ agecat + sex + season +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = hex.agg.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

# Summarize results
summary(mod)
result.mod <- mod$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod$pvalue <- 2*(1 - pnorm(abs(mod$summary.fixed[,"mean"]/mod$summary.fixed[,"sd"])))
result.mod

# Plot random effects
plot(cbind(hex.sf, car = mod$summary.random$hex.car[, "mean"])[, "car"],
  breaks = "fisher", nbreaks = 50,
  pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
  key.pos = 1,
  lwd = 0.1)
plot(cbind(hex.sf, iid = mod$summary.random$hex.iid[, "mean"])[, "iid"],
  breaks = "fisher", nbreaks = 50,
  pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
  key.pos = 1,
  lwd = 0.1)

#
# Univariate models with animals --------------------------------------------------------------------
#

# Join hex.agg.data with animal.cat.hex.sf
tmp.data <- full_join(hex.agg.data, animal.hex.sf)

#including relevel function
tmp.data$agecat<-relevel(tmp.data$agecat, ref = "[50,Inf]")
#tmp.data.cat$agecat<-relevel(tmp.data$agecat, ref = "[50,Inf]")

# Univariate analyses
# Age
mod1 <- inla(
  formula = cases ~ agecat +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod1)
result.mod1 <- mod1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod1$pvalue <- 2*(1 - pnorm(abs(mod1$summary.fixed[,"mean"]/mod1$summary.fixed[,"sd"])))
result.mod1

# Plot random effects
plot(cbind(hex.sf, car = mod1$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod1$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Sex
mod2 <- inla(
  formula = cases ~ sex +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod2)
result.mod2 <- mod2$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod2$pvalue <- 2*(1 - pnorm(abs(mod2$summary.fixed[,"mean"]/mod2$summary.fixed[,"sd"])))
result.mod2

# Plot random effects
plot(cbind(hex.sf, car = mod2$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod2$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Season
tmp.data$season <-relevel(tmp.data$season, ref = "Winter")
mod3 <- inla(
  formula = cases ~ season +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod3)
result.mod3 <- mod3$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod3$pvalue <- 2*(1 - pnorm(abs(mod3$summary.fixed[,"mean"]/mod3$summary.fixed[,"sd"])))
result.mod3

# Plot random effects
plot(cbind(hex.sf, car = mod3$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod3$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Poultry
mod4 <- inla(
  formula = cases ~ log2(Pluimvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod4)
result.mod4 <- mod4$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod4$pvalue <- 2*(1 - pnorm(abs(mod4$summary.fixed[,"mean"]/mod4$summary.fixed[,"sd"])))
result.mod4

# Plot random effects
plot(cbind(hex.sf, car = mod4$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod4$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Pigs
mod5 <- inla(
  formula = cases ~ log2(Varkens_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod5)
result.mod5 <- mod5$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod5$pvalue <- 2*(1 - pnorm(abs(mod5$summary.fixed[,"mean"]/mod5$summary.fixed[,"sd"])))
result.mod5

# Plot random effects
plot(cbind(hex.sf, car = mod5$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod5$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Cattle
mod6 <- inla(
  formula = cases ~ log2(Rundvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod6)
result.mod6 <- mod6$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6$pvalue <- 2*(1 - pnorm(abs(mod6$summary.fixed[,"mean"]/mod6$summary.fixed[,"sd"])))
result.mod6

# Plot random effects
plot(cbind(hex.sf, car = mod6$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod6$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Dairy cows
mod6 <- inla(
  formula = cases ~ log2(Rund_Melk_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod6)
result.mod6 <- mod6$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6$pvalue <- 2*(1 - pnorm(abs(mod6$summary.fixed[,"mean"]/mod6$summary.fixed[,"sd"])))
result.mod6

# Plot random effects
plot(cbind(hex.sf, car = mod6$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod6$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Veal calves
mod6 <- inla(
  formula = cases ~ log2(Rund_Vlees_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod6)
result.mod6 <- mod6$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6$pvalue <- 2*(1 - pnorm(abs(mod6$summary.fixed[,"mean"]/mod6$summary.fixed[,"sd"])))
result.mod6

# Plot random effects
plot(cbind(hex.sf, car = mod6$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod6$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Goats
mod7 <- inla(
  formula = cases ~ log2(Geiten_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod7)
result.mod7 <- mod7$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod7$pvalue <- 2*(1 - pnorm(abs(mod7$summary.fixed[,"mean"]/mod7$summary.fixed[,"sd"])))
result.mod7

# Plot random effects
plot(cbind(hex.sf, car = mod7$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod7$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Sheep
mod8 <- inla(
  formula = cases ~ log2(Schapen_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod8)
result.mod8 <- mod8$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod8$pvalue <- 2*(1 - pnorm(abs(mod8$summary.fixed[,"mean"]/mod8$summary.fixed[,"sd"])))
result.mod8

# Plot random effects
plot(cbind(hex.sf, car = mod8$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod8$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# Small ruminants
mod9 <- inla(
  formula = cases ~ log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod9)
result.mod9 <- mod9$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod9$pvalue <- 2*(1 - pnorm(abs(mod9$summary.fixed[,"mean"]/mod9$summary.fixed[,"sd"])))
result.mod9

# Plot random effects
plot(cbind(hex.sf, car = mod9$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod9$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

# --------------------------------------------------------------------------------------------------------------------------
# Multivariate analyses small ruminants + season

#select summer
tmp.data.summer <- subset(tmp.data, tmp.data$season == 'summer')

# analysis summer
mod13 <- inla(
  formula = cases ~ sex + agecat + log2(Pluimvee_totaal + 1) + log2(Varkens_totaal + 1) +
    log2(Rundvee_totaal + 1) + log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.summer,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod13)
result.mod13 <- mod13$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod13$pvalue <- 2*(1 - pnorm(abs(mod13$summary.fixed[,"mean"]/mod13$summary.fixed[,"sd"])))
result.mod13

# Plot random effects
plot(cbind(hex.sf, car = mod13$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Greys", n = 9))),
     key.pos = 1,
     lwd = 0.1,
     main = "CAR - summer")
plot(cbind(hex.sf, iid = mod13$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Greys", n = 9))),
     key.pos = 1,
     lwd = 0.1,
     main = "IID - summer")

#select winter
tmp.data.winter <- subset(tmp.data, tmp.data$season == 'winter')
#tmp.data.cat.winter <- subset(tmp.data.cat, tmp.data$season == 'winter')

# analysis winter
mod14 <- inla(
  formula = cases ~ sex + agecat + log2(Pluimvee_totaal + 1) + log2(Varkens_totaal + 1) +
    log2(Rundvee_totaal + 1) + log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.winter,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod14)
result.mod14 <- mod14$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod14$pvalue <- 2*(1 - pnorm(abs(mod14$summary.fixed[,"mean"]/mod14$summary.fixed[,"sd"])))
result.mod14

# Plot random effects
plot(cbind(hex.sf, car = mod14$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Greys", n = 9))),
     key.pos = 1,
     lwd = 0.1,
     main = "CAR - winter")
plot(cbind(hex.sf, iid = mod14$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Greys", n = 9))),
     key.pos = 1,
     lwd = 0.1,
     main = "IID - winter")

#------------------------------------------------------------------------------------------------------

# Model without small ruminants (only poultry, pigs and cattle)

#select summer
tmp.data.summer <- subset(tmp.data, tmp.data$season == 'summer')

# analysis summer
mod15 <- inla(
  formula = cases ~ sex + agecat + log2(Pluimvee_totaal + 1) + log2(Varkens_totaal + 1) +
    log2(Rundvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.summer,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod15)
result.mod15 <- mod15$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod15$pvalue <- 2*(1 - pnorm(abs(mod15$summary.fixed[,"mean"]/mod15$summary.fixed[,"sd"])))
result.mod15

# Plot random effects
plot(cbind(hex.sf, car = mod15$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod15$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

#select winter
tmp.data.winter <- subset(tmp.data, tmp.data$season == 'winter')
#tmp.data.cat.winter <- subset(tmp.data.cat, tmp.data$season == 'winter')

# analysis winter
mod16 <- inla(
  formula = cases ~ sex + agecat + log2(Pluimvee_totaal + 1) + log2(Varkens_totaal + 1) +
    log2(Rundvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.winter,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod16)
result.mod16 <- mod16$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod16$pvalue <- 2*(1 - pnorm(abs(mod16$summary.fixed[,"mean"]/mod16$summary.fixed[,"sd"])))
result.mod16

# Plot random effects
plot(cbind(hex.sf, car = mod16$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod16$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

#------------------------------------------------------------------------------------------------------------------------

# Model only cattle


#select summer
tmp.data.summer <- subset(tmp.data, tmp.data$season == 'summer')

# analysis summer
mod17 <- inla(
  formula = cases ~ sex + agecat + log2(Rundvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.summer,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod17)
result.mod17 <- mod17$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod17$pvalue <- 2*(1 - pnorm(abs(mod17$summary.fixed[,"mean"]/mod17$summary.fixed[,"sd"])))
result.mod17

# Plot random effects
plot(cbind(hex.sf, car = mod17$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod17$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)

#select winter
tmp.data.winter <- subset(tmp.data, tmp.data$season == 'winter')
#tmp.data.cat.winter <- subset(tmp.data.cat, tmp.data$season == 'winter')

# analysis winter
mod18 <- inla(
  formula = cases ~ sex + agecat + log2(Rundvee_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.winter,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod18)
result.mod18 <- mod18$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod18$pvalue <- 2*(1 - pnorm(abs(mod18$summary.fixed[,"mean"]/mod18$summary.fixed[,"sd"])))
result.mod18

# Plot random effects
plot(cbind(hex.sf, car = mod18$summary.random$hex.car[, "mean"])[, "car"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
plot(cbind(hex.sf, iid = mod18$summary.random$hex.iid[, "mean"])[, "iid"],
     breaks = "fisher", nbreaks = 50,
     pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
     key.pos = 1,
     lwd = 0.1)
