library(tigris)
library(sf)
library(tidyverse)

### Census Tracts 2010

# Set year and avoid redownloading
options(tigris_use_cache = TRUE, tigris_class = "sf", tigris_year = 2010)

# Define directory to save shapefiles
dir.create("shapefiles/tracts_2010", recursive = TRUE, showWarnings = FALSE)

# Download and save tracts for each state
states <- unique(fips_codes$state)[1:51] 

for (state in states) {
  shp <- tracts(state = state, year = 2010, protocol = "ftp")
  
  # transform to WGS84 before saving
  shp <- st_transform(shp, 4326)
  
  st_write(shp, 
           paste0("shapefiles/tracts_2010/tracts_", state, ".gpkg"), 
           delete_layer = TRUE)
}




### ZCTA 2010

# Set tigris options for 2010
options(tigris_use_cache = TRUE, tigris_class = "sf", tigris_year = 2010)

# Create output directory
dir.create("shapefiles/zctas_2010", recursive = TRUE, showWarnings = FALSE)

# Download the full national 2010 ZCTA shapefile
zcta_2010 <- zctas(year = 2010, protocol = "ftp")

# transform to WGS84 before saving
zcta_2010 <- st_transform(zcta_2010, 4326)

# Save as GeoPackage
st_write(zcta_2010, "shapefiles/zctas_2010/zctas_2010.gpkg", delete_layer = TRUE)




### Census Tracts 2000

# Set year and avoid redownloading
options(tigris_use_cache = TRUE, tigris_class = "sf", tigris_year = 2000)

# Define directory to save shapefiles
dir.create("shapefiles/tracts_2000", recursive = TRUE, showWarnings = FALSE)

# Download and save tracts for each state
states <- unique(fips_codes$state)[1:51] 

for (state in states) {
  shp <- tracts(state = state, year = 2000, protocol = "ftp")
  
  # transform to WGS84 before saving
  shp <- st_transform(shp, 4326)
  
  st_write(shp, 
           paste0("shapefiles/tracts_2000/tracts_", state, ".gpkg"), 
           delete_layer = TRUE)
}




### ZCTA 2000

# Set tigris options for 2000
options(tigris_use_cache = TRUE, tigris_class = "sf", tigris_year = 2000)

# Create output directory
dir.create("shapefiles/zctas_2000", recursive = TRUE, showWarnings = FALSE)

# Download the full national 2000 ZCTA shapefile
zcta_2000 <- zctas(year = 2000, protocol = "ftp")

# transform to WGS84 before saving
zcta_2000 <- st_transform(zcta_2000, 4326)

# Save as GeoPackage
st_write(zcta_2000, "shapefiles/zctas_2000/zctas_2000.gpkg", delete_layer = TRUE)