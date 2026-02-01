library(tidycensus)
library(dplyr)
library(purrr)
library(writexl)

# Set your Census API key
census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE)

# Define variables of interest
variables <- c(
  "B17021_002E", "B17021_001E", "B17010_002E", "B17010_001E", "B19013_001E",
  "B23001_006E", "B23001_008E", "B23001_013E", "B23001_015E", "B23001_020E",
  "B23001_022E", "B23001_027E", "B23001_029E", "B23001_034E", "B23001_036E",
  "B23001_041E", "B23001_043E", "B23001_048E", "B23001_050E", "B23001_055E",
  "B23001_057E", "B23001_062E", "B23001_064E", "B23001_069E", "B23001_071E",
  "B23001_074E", "B23001_076E", "B23001_079E", "B23001_081E", "B23001_084E",
  "B23001_086E", "B23001_092E", "B23001_094E", "B23001_099E", "B23001_101E",
  "B23001_106E", "B23001_108E", "B23001_113E", "B23001_115E", "B23001_120E",
  "B23001_122E", "B23001_127E", "B23001_129E", "B23001_134E", "B23001_136E",
  "B23001_141E", "B23001_143E", "B23001_148E", "B23001_150E", "B23001_155E",
  "B23001_157E", "B23001_160E", "B23001_162E", "B23001_165E", "B23001_167E",
  "B23001_170E", "B23001_172E", "B25014_001E", "B25014_002E", "B25014_005E",
  "B25014_006E", "B25014_007E", "B25014_008E", "B25014_011E", "B25014_012E",
  "B25014_013E", "B25106_001E", "B25106_006E", "B25106_010E", "B25106_014E",
  "B25106_018E", "B25106_022E", "B25106_028E", "B25106_032E", "B25106_036E",
  "B25106_040E", "B25106_044E"
)

## CENSUS

# --- Get state FIPS codes from tidycensus ---
states <- unique(fips_codes$state)[!unique(fips_codes$state) %in% c("PR", "AS", "GU", "MP", "VI", "UM")]

# --- Loop through all states ---
all_tracts_data <- map_dfr(states, function(st) {
  tryCatch({
    get_acs(
      geography = "tract",
      variables = variables,
      year = 2013,
      survey = "acs5",
      output = "wide",
      state = st
    )
  }, error = function(e) {
    message(paste("Failed for state:", st, "Error:", e$message))
    NULL
  })
})

# --- Inspect & save ---
glimpse(all_tracts_data)

write_xlsx(all_tracts_data, "ACS2013Census2010.xlsx")

## ZCTA

setwd('/Users/nisingh/Documents/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/ZCTA 2010')

# --- Loop through all ZCTAs ---
all_zcta_data <- get_acs(
  geography = "zcta",
  variables = variables,
  year = 2013,        # ACS 5-year estimates (earliest is 2009–2013 window)
  survey = "acs5",
  output = "wide"
)

# Inspect and save
glimpse(all_zcta_data)

all_zcta_data <- all_zcta_data %>% mutate(ZCTA = substr(GEOID, 3, 7))

write_xlsx(all_zcta_data, "ACS2013ZCTA2010.xlsx")