library(tidycensus)
library(tidyverse)
library(writexl)

census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE, overwrite = TRUE)

# State list (incl. DC)
states <- c(state.abb, "DC")

# Adult variables (18+)
adult_vars <- c(
  paste0("P012", sprintf("%03d", 7:25)),  # Male 18+
  paste0("P012", sprintf("%03d", 31:49)) # Female 18+
)

# Total population variable
total_var <- "P012001"

# Function to fetch data
get_tract_data <- function(state) {
  get_decennial(
    geography = "tract",
    variables = c(total_var, adult_vars),
    state = state,
    year = 2010,
    sumfile = "sf1",
    geometry = FALSE
  ) %>%
    group_by(GEOID, NAME) %>%
    mutate(variable = ifelse(variable == total_var, "total_population", "adult_component")) %>%
    summarise(
      total_population = sum(value[variable == "total_population"]),
      adult_population = sum(value[variable == "adult_component"]),
      .groups = "drop"
    )
}

# Fetch for all states
all_tracts <- map_dfr(states, get_tract_data)

# View result
head(all_tracts)

write_xlsx(all_tracts, "us_census_tract_population_2010.xlsx")
