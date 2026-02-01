# CENSUS

census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE, overwrite = TRUE)

# Set your Census API key (already done, included here for completeness)
census_api_key("1b02ea33fad73affe632747d96d0c5369450473f", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# Get vector of all state FIPS codes from tidycensus
all_states <- unique(fips_codes$state)[1:51]  # 50 states + DC (exclude PR)

# Function to download data for one state
get_state_poverty_data <- function(state_abbr) {
  tryCatch({
    get_acs(
      geography = "tract",
      variables = c(
        individual_total = "B17021_001E",
        individual_below_poverty = "B17021_002E",
        family_total = "B17010_001E",
        family_below_poverty = "B17010_002E",
        total_family_households = "B11001_002E"
      ),
      year = 2018,
      survey = "acs5",
      state = state_abbr,
      geometry = FALSE,
      cache_table = TRUE
    )
  }, error = function(e) {
    message(paste("Failed for", state_abbr, ":", e$message))
    return(NULL)
  })
}

# Loop over all states and combine into one dataframe
poverty <- map_dfr(all_states, get_state_poverty_data)

write_xlsx(poverty, path = "poverty.xlsx")

# SPI
library(httr)
library(jsonlite)
library(dplyr)

# ArcGIS REST API endpoint
base_url <- "https://egis.hud.gov/arcgis/rest/services/affh/AffhtMapService/MapServer/15/query"

# Setup
batch_size <- 5000
offset <- 0
all_data <- list()
batch <- 1

repeat {
  # Use WHERE clause to filter by VERSION
  params <- list(
    where = "VERSION = 'AFFHT0006'",
    outFields = "*",
    returnGeometry = "false",
    f = "json",
    resultOffset = offset,
    resultRecordCount = batch_size
  )
  
  # Request data
  res <- GET(base_url, query = params)
  json_data <- fromJSON(content(res, "text"), flatten = TRUE)
  
  # Stop if no data
  if (is.null(json_data$features) || nrow(json_data$features) == 0) {
    cat("✅ Done. No more data at offset", offset, "\n")
    break
  }
  
  # Extract and clean
  df <- as.data.frame(json_data$features)
  names(df) <- sub("^attributes\\.", "", names(df))  # Remove "attributes." prefix
  
  all_data[[batch]] <- df
  cat("📦 Batch", batch, "- downloaded", nrow(df), "records\n")
  
  # If fewer than 5000 rows, this is the last batch
  if (nrow(df) < batch_size) break
  
  offset <- offset + batch_size
  batch <- batch + 1
}

# Combine and export
SPI <- bind_rows(all_data)
write.csv(SPI, "school_proficiency_index_AAAAI.csv", row.names = FALSE)
cat("✅ Saved", nrow(SPI), "filtered rows to school_proficiency_index.csv\n")