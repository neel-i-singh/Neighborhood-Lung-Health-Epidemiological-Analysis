library(readxl)
library(tidyverse)

census <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/COI 2.0 census.xlsx')
census_population <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/COI 2.0 Census Population.xlsx')
zip <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/COI 2.0 2020 zip.xlsx')


tractzip2020q1 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/TRACT_ZIP_032020.xlsx')
tractzip2020q2 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/TRACT_ZIP_062020.xlsx')
tractzip2020q3 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/TRACT_ZIP_092020.xlsx')
tractzip2020q4 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/TRACT_ZIP_122020.xlsx')

ziptract2020q1 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/ZIP_TRACT_032020.xlsx')
ziptract2020q2 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/ZIP_TRACT_062020.xlsx')
ziptract2020q3 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/ZIP_TRACT_092020.xlsx')
ziptract2020q4 <- read_xlsx('/Users/nisingh/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Neighborhood Lung Health Epidemiological Analysis/Neighborhood data/Testing Census to ZIP Crosswalk/ZIP_TRACT_122020.xlsx')

# Function: aggregate tract-level COI 2.0 to ZIP-level
crosswalk_coi_zip_pop <- function(census_df, census_pop_df, ziptract_df, tractzip_df) {
  
  ## --- COI crosswalk (use ziptract) ---
  
  # 1. Keep only 2015 tract COI z-scores
  census_2015 <- census_df %>%
    filter(year == 2015) %>%
    select(geoid, starts_with("z_"))
  
  # 2. Join tract COI with ZIP-TRACT crosswalk
  joined_coi <- ziptract_df %>%
    left_join(census_2015, by = c("TRACT" = "geoid"))
  
  # 3. Rescale weights
  joined_coi <- joined_coi %>%
    group_by(ZIP, qtr) %>%
    mutate(qtr_weight = TOT_RATIO / sum(TOT_RATIO, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ZIP) %>%
    mutate(rescaled_wt = qtr_weight / sum(qtr_weight, na.rm = TRUE)) %>%
    ungroup()
  
  # 4. Aggregate weighted z-scores
  zip_scores <- joined_coi %>%
    group_by(ZIP) %>%
    summarise(
      z_ED_nat  = sum(rescaled_wt * z_ED_nat,  na.rm = TRUE),
      z_HE_nat  = sum(rescaled_wt * z_HE_nat,  na.rm = TRUE),
      z_SE_nat  = sum(rescaled_wt * z_SE_nat,  na.rm = TRUE),
      z_COI_nat = sum(rescaled_wt * z_COI_nat, na.rm = TRUE)
    )
  
  ## --- Population crosswalk (use tractzip) ---
  
  # 5. Population data at tract level
  census_pop <- census_pop_df %>%
    filter(year == 2015) %>%
    select(geoid, pop, aian, api, black, hisp, other2, nonwhite, white, total)
  
  # 6. Join tract population with TRACT→ZIP crosswalk
  joined_pop <- tractzip_df %>%
    left_join(census_pop, by = c("TRACT" = "geoid"))
  
  # 7. Rescale weights for TRACT→ZIP
  # 7. Use tract-based TOT_RATIO directly (normalize within tract only)
  joined_pop <- joined_pop %>%
    group_by(TRACT, qtr) %>%
    mutate(qtr_weight = TOT_RATIO / sum(TOT_RATIO, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(TRACT) %>%
    mutate(rescaled_wt = qtr_weight / sum(qtr_weight, na.rm = TRUE)) %>%
    ungroup()
  
  # 8. Aggregate weighted population counts
  zip_pop <- joined_pop %>%
    group_by(ZIP) %>%
    summarise(
      pop      = sum(rescaled_wt * pop,      na.rm = TRUE),
      aian     = sum(rescaled_wt * aian,     na.rm = TRUE),
      api      = sum(rescaled_wt * api,      na.rm = TRUE),
      black    = sum(rescaled_wt * black,    na.rm = TRUE),
      hisp     = sum(rescaled_wt * hisp,     na.rm = TRUE),
      other2   = sum(rescaled_wt * other2,   na.rm = TRUE),
      #nonwhite = sum(rescaled_wt * nonwhite, na.rm = TRUE),
      white    = sum(rescaled_wt * white,    na.rm = TRUE)
      #total    = sum(rescaled_wt * total,    na.rm = TRUE)
    )
  
  ## --- Merge COI and population ---
  out <- zip_scores %>%
    left_join(zip_pop, by = "ZIP")
  
  return(out)
}

# Stack crosswalks
ziptract2020 <- bind_rows(
  ziptract2020q1 %>% mutate(qtr = "2020q1"),
  ziptract2020q2 %>% mutate(qtr = "2020q2"),
  ziptract2020q3 %>% mutate(qtr = "2020q3"),
  ziptract2020q4 %>% mutate(qtr = "2020q4")
) %>% filter(!grepl("^(72|78|66|69|60)", TRACT))

tractzip2020 <- bind_rows(
  tractzip2020q1 %>% mutate(qtr = "2020q1"),
  tractzip2020q2 %>% mutate(qtr = "2020q2"),
  tractzip2020q3 %>% mutate(qtr = "2020q3"),
  tractzip2020q4 %>% mutate(qtr = "2020q4")
) %>% filter(!grepl("^(72|78|66|69|60)", TRACT))

# Run function
zip_scores_2020 <- crosswalk_coi_zip_pop(census, census_population, ziptract2020, tractzip2020)

# Restrict + rank
library(Hmisc)  # for wtd.quantile

# Function: population-weighted COI score (1–100)
weighted_coi_score <- function(z, pop) {
  # Compute 0–100th percentiles (inclusive)
  qtiles <- wtd.quantile(z, weights = pop, probs = seq(0, 1, 0.01), type = "i/n")
  
  # Use these as cutpoints (101 values → 100 bins)
  scores <- cut(
    z,
    breaks = qtiles,
    labels = 1:100,
    include.lowest = TRUE,
    right = TRUE
  )
  
  as.numeric(as.character(scores))
}

zip_scores_2020 <- zip_scores_2020 %>%
  mutate(
    r_ED_nat  = weighted_coi_score(z_ED_nat,  pop),
    r_HE_nat  = weighted_coi_score(z_HE_nat,  pop),
    r_SE_nat  = weighted_coi_score(z_SE_nat,  pop),
    r_COI_nat = weighted_coi_score(z_COI_nat, pop)
  )


