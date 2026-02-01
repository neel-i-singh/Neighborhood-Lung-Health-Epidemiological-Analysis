library(tidycensus)
library(dplyr)
library(purrr)
library(writexl)

# Set your Census API key
census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE)

# Total child variables
total_child_vars <- c(
  "B01001_003E", "B01001_004E", "B01001_005E", "B01001_006E",  # Male <18
  "B01001_027E", "B01001_028E", "B01001_029E", "B01001_030E"   # Female <18
)

# Black only child variables
black_child_vars <- c(
  "B01001B_003E", "B01001B_004E", "B01001B_005E", "B01001B_006E",  # Male <18
  "B01001B_018E", "B01001B_019E", "B01001B_020E", "B01001B_021E"   # Female <18
)

# Hispanic child variables
hispanic_child_vars <- c(
  "B01001I_003E", "B01001I_004E", "B01001I_005E", "B01001I_006E",
  "B01001I_018E", "B01001I_019E", "B01001I_020E", "B01001I_021E"
)

# White only child variables
white_child_vars <- c(
  "B01001A_003E", "B01001A_004E", "B01001A_005E", "B01001A_006E",
  "B01001A_018E", "B01001A_019E", "B01001A_020E", "B01001A_021E"
)

# Combined list
variables <- c(
  total_child_vars,
  black_child_vars,
  hispanic_child_vars,
  white_child_vars
)



## CENSUS

setwd('/Users/nisingh/Documents/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Census 2010')

# --- Get state FIPS codes from tidycensus ---
states <- unique(fips_codes$state)[!unique(fips_codes$state) %in% c("PR", "AS", "GU", "MP", "VI", "UM")]  # exclude Puerto Rico if not needed

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

write_xlsx(all_tracts_data, "ACS2013ChildPopulationCensus2010.xlsx")

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

write_xlsx(all_zcta_data, "ACS2013ChildPopulationZCTA2010.xlsx")
