# Load libraries
library(tidycensus)
library(tidyverse)

# Set your Census API key
census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE)

# Define ACS variables (B03002 table)
acs_vars <- c(
  total   = "B03002_001",
  white   = "B03002_003",
  black   = "B03002_004",
  aian    = "B03002_005",
  api     = "B03002_006",
  nhpi    = "B03002_007",
  other   = "B03002_008",
  multi   = "B03002_009",
  hisp    = "B03002_012"
)

# Example: North Carolina (change to your state of interest)
tract_pop_nc <- get_acs(
  geography = "tract",
  state = "NC",           # REQUIRED for tracts
  variables = acs_vars,
  year = 2017,
  survey = "acs5",
  output = "wide"
)

tract_pop_nc <- tract_pop_nc %>%
  transmute(
    geoid    = GEOID,
    pop      = totalE,
    aian     = aianE,
    api      = apiE + nhpiE,
    black    = blackE,
    hisp     = hispE,
    other2   = otherE + multiE,
    white    = whiteE,
    nonwhite = totalE - whiteE,
    total    = totalE
  )

head(tract_pop_nc)
