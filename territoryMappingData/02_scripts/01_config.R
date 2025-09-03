## -----------------------------
## TABMON_dataprep.Rproject > territoryMappingData/
## 01_config.R
## 🔧 Sets up the environment (libraries, paths, helper functions)
## -----------------------------


# --- Libraries ---
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(leaflet)
library(purrr)
library(readr)
library(sf)
library(tidyr)
library(lubridate)
library(hms)
library(here)

# --- Paths ---
project_root            <- here::here()
data_raw                <- file.path(project_root, "territoryMappingData", "01_data", "raw")
data_proc               <- file.path(project_root, "territoryMappingData", "01_data", "processed")
translated_data_path    <- file.path(data_proc, "02_translated_data.rds")
data_with_clusters_path <- file.path(data_proc, "03_data_with_clusters.rds")
data_enriched_path      <- file.path(data_proc, "04_data_enriched.rds")
species_key <- read.csv("C:/Users/cbarile/My Drive/PostDoc UvA/TABMON/Species_lists/Species_names_keys/MULTILINGUAL_BIRD_KEY.csv")
bugg_sheet_url          <- "https://docs.google.com/spreadsheets/d/1Wqz7g0I2cqoMFSI-CkN3Fss1h62ceoos6_OtARi490c/edit?gid=208765239#gid=208765239" # BUGG sheet URL

# --- Helper functions ---

#############################################
### Cleaning columns and names
#############################################
clean_data <- function(data, rename_map = NULL, clean_name_cols = NULL, drop_cols = NULL) {
  # Rename columns if a map is provided
  if (!is.null(rename_map)) {
    data <- data %>%
      rename_with(~ rename_map[.x], .cols = names(rename_map)[names(rename_map) %in% names(data)])
  }
  # Clean specified name columns
  if (!is.null(clean_name_cols)) {
    for (col in clean_name_cols) {
      if (col %in% names(data)) {
        data <- data %>%
          mutate(!!sym(col) := !!sym(col) %>%
                   gsub("[,:]", "", .) %>%    # remove commas/colons
                   gsub("\\s+", "_", .) %>%   # spaces -> underscores
                   gsub("_+", "_", .) %>%     # collapse multiple underscores
                   tolower()                  # lowercase
          )
      }
    }
  }
  # Drop any additional unwanted columns (ignore if not present)
  if (!is.null(drop_cols)) {
    data <- data %>% select(-any_of(drop_cols))
  }
  return(data)
}

#############################################
## Define rename mappings and drop lists
#############################################
rename_overview <- c(
  projectid = "project_id",
  plotid = "plot_id",
  naam = "plot_name",
  bzdid = "visit_id",
  datum = "date",
  begintijd = "start_time",
  eindtijd = "end_time",
  bezoekduur = "visit_duration",
  aantal_minuten = "visit_duration_minutes",
  gunstig = "favorable_conditions",
  omst_opm = "conditions_notes",
  opm = "notes",
  waarnemerid = "observer_id",
  jaar = "year",
  doy = "day_of_year",
  nsoort = "n_species",
  nrecord = "n_records"
)

rename_plots <- c(
  plotid = "plot_id",
  projectid = "project_id",
  plotnummer = "plot_number",
  naam = "plot_name",
  opp_ha = "area_ha"
)

rename_transects <- c(
  projectid = "project_id",
  plotid = "plot_id",
  kopid = "track_id",
  jaar = "year",
  bzdid = "visit_id",
  begintijd = "start_time",
  eindtijd = "end_time"
)

rename_visits <- c(
  projectid = "project_id",
  kopid = "track_id",
  plotid = "plot_id",
  id = "obs_id",
  x_coord = "x_coord",
  y_coord = "y_coord",
  soortgrp = "species_group",
  soortnr = "species_code",
  naam = "species_name",
  aantal = "count",
  jaar = "year",
  maand = "month",
  dag = "day",
  doy = "day_of_year",
  wrntype = "observation_type",
  broedcode = "breeding_code",
  opmerk = "notes",
  clterr = "territory_cluster",
  clterrid = "cluster_id",
  inplot = "in_plot",
  ioc_sort = "ioc_sort_order",
  geslacht = "sex",
  bzdid = "visit_id"
)

# Geslacht/Sex codes:
# (likely, not confirmed) m = male, v = female, o = undetermined, p = pair

rename_territories <- c(
  projectid = "project_id",
  plotid = "plot_id",
  kopid = "track_id",
  plotnummer = "plot_number",
  plotnaam = "plot_name",
  jaar = "year",
  euring = "species_code",
  naam = "species_name",
  aantal = "territory_count",
  broedcode = "breeding_code",
  opmerking = "notes",
  inplot = "in_plot",
  x_coord = "x_coord",
  y_coord = "y_coord"
)

#############################################
## Clean up non-bird species
#############################################

detect_species_mismatches <- function(data, species_key,
                                      sp_code_col = "species_code",
                                      sp_name_col = "species_name",
                                      language = "dutch",
                                      return_unique_sp = FALSE) {
  
  # 1.Define supported languages and their columns
  supported_languages <- c("dutch", "english", "french", "spanish", "norwegian")
  language_cols <- list(
    dutch     = c("nl_ioc", "nl_birdnet"),
    english   = c("en_uk_birdnet", "clem_common", "ioc_common"),
    french    = c("fr_ioc", "fr_birdnet"),
    spanish   = c("es_ioc", "es_birdnet"),
    norwegian = c("no_ioc", "no_birdnet")
  )
  
  if (!(language %in% supported_languages)) {
    stop(paste0("Language not available in the species key.\nSupported options are: ", 
                paste(supported_languages, collapse = ", ")))
  }
  
  # 2. Create a vector of all valid EURING codes (from the species key)
  valid_sp_codes <- unique(species_key$euring)
  
  # 3. Create a vector of all valid species names for the selected language
  valid_sp_names <- unique(unlist(species_key[language_cols[[language]]]))
  
  # 4. Filter rows where both conditions are true:
  # - The species code is NOT in the valid EURING list
  # - The species name is NOT in the valid name lists for the selected language
  # Such rows should be flagged as suspicious/mismatched
  
  mismatched <- data %>%
    mutate(!!sym(sp_name_col) := tolower(!!sym(sp_name_col))) %>%
    # sym() takes a string column name and turns it into a symbol usable by dplyr inside pipes
    # !! unquotes, injects that symbol to be evaluated as a column reference inside filter()
    filter(!(!!sym(sp_code_col) %in% valid_sp_codes) &       # Species code mismatch
             !(!!sym(sp_name_col) %in% valid_sp_names))        # Species name mismatch
  
  # 5. Return the detected mismatches
  if (return_unique_sp) {
    return(mismatched %>% distinct(!!sym(sp_code_col), !!sym(sp_name_col)))
  } else {
    return(mismatched)}
}

remove_non_bird <- function(data, species_name_col = "species_name", non_birds_list) {
  col_sym <- sym(species_name_col) # Ensure case-insensitive comparison

  birds_only <- data %>%
    mutate(.tmp_species = tolower(!!col_sym)) %>%
    filter(!.tmp_species %in% tolower(non_birds_list)) %>%
    select(-.tmp_species)
  
  n_removed <- nrow(data) - nrow(birds_only)

  message(glue::glue("Removed {n_removed} non-bird records from source data."))
  # Return invisibly (output, but not showing it)
  invisible(birds_only)
}

#############################################
# Translate bird species to English
#############################################
species_translation <- function(data, species_key,
                                sp_code_col = "species_code",
                                sp_name_col = "species_name",
                                language = "dutch") {
  
  # 1. Standardise column names in species key
  species_key <- rename_with(species_key, tolower)
  
  # Add english_name using preferred order
  species_key <- species_key %>%
    mutate(english_name = coalesce(en_uk_birdnet, clem_common, ioc_common))
  
  # 2. Define supported languages and name columns
  supported_languages <- c("dutch", "english", "french", "spanish", "norwegian")
  language_cols <- list(
    dutch     = c("nl_ioc", "nl_birdnet"),
    english   = c("en_uk_birdnet", "clem_common", "ioc_common"),
    french    = c("fr_ioc", "fr_birdnet"),
    spanish   = c("es_ioc", "es_birdnet"),
    norwegian = c("no_ioc", "no_birdnet")
  )
  
  if (!(language %in% supported_languages)) {
    stop(paste0("Language not available.\nSupported: ", 
                paste(supported_languages, collapse = ", ")))
  }
  
  # 3. Pivot species_key for matching
  name_cols <- language_cols[[language]]
  
  species_key_filtered <- species_key %>%
    select(euring_code, scientific_name, english_name, all_of(name_cols)) %>%
    pivot_longer(cols = all_of(name_cols),
                 names_to = "name_source",
                 values_to = "match_name") %>%
    filter(!is.na(match_name)) %>%
    distinct(euring_code, scientific_name, english_name, match_name, name_source)
  
  # 4. Prepare input data
  geometry <- sf::st_geometry(data)
  data_nogeom <- sf::st_set_geometry(data, NULL) %>%
    rename(code = !!sym(sp_code_col),
           name = !!sym(sp_name_col)) %>%
    mutate(row_id = row_number(),
           name = tolower(name))  # ensure lowercase for matching
  
  # 5. Phase 1: Match by code AND name
  matched_long <- data_nogeom %>%
    inner_join(species_key_filtered,
               by = c("code" = "euring_code", "name" = "match_name")) %>%
    mutate(match_type = "both")
  
  matched_sources <- matched_long %>%
    group_by(row_id) %>%
    summarize(
      matched_name_source = paste(sort(unique(name_source)), collapse = ", "),
      scientific_name = first(scientific_name),
      english_name = first(english_name),
      match_type = first(match_type),
      .groups = "drop"
    )
  
  matched_full <- data_nogeom %>%
    inner_join(matched_sources, by = "row_id")
  
  matched_full$geometry <- geometry[matched_full$row_id]
  matched_sf <- sf::st_as_sf(matched_full, sf_column_name = "geometry", crs = sf::st_crs(data))
  
  matched_species <- matched_sf %>% distinct(code, name)
  unmatched_species <- anti_join(data_nogeom, matched_sf, by = "row_id") %>%
    distinct(row_id, code, name)
  
  cat("✅ ", nrow(matched_species), " unique species matched by both euring code and name.\n")
  cat("⚠️ ", nrow(distinct(unmatched_species, code, name)), " unmatched species remain.\n")
  View(matched_species)
  View(unmatched_species %>% distinct(code, name))
  
  proceed <- readline("When ready to proceed to Phase 2 (match by code OR name), type 'Ready': ")
  if (tolower(proceed) != "ready") {
    cat("❌ User cancelled Phase 2. Returning only Phase 1 results.\n")
    return(list(
      final_visits_sf = matched_sf,
      matched_species = matched_species,
      unmatched_species = unmatched_species
    ))
  }
  
  # 6. Phase 2: Match by code OR name
  matched_by_name_long <- unmatched_species %>%
    inner_join(species_key_filtered, by = c("name" = "match_name")) %>%
    mutate(match_type = "name")
  
  matched_by_name_sources <- matched_by_name_long %>%
    group_by(row_id) %>%
    summarize(
      matched_name_source = paste(sort(unique(name_source)), collapse = ", "),
      scientific_name = first(scientific_name),
      english_name = first(english_name),
      match_type = first(match_type),
      .groups = "drop"
    )
  
  matched_by_name_full <- unmatched_species %>%
    inner_join(matched_by_name_sources, by = "row_id")
  
  matched_by_code_long <- unmatched_species %>%
    inner_join(species_key_filtered, by = c("code" = "euring_code")) %>%
    mutate(match_type = "code")
  
  matched_by_code_sources <- matched_by_code_long %>%
    group_by(row_id) %>%
    summarize(
      matched_name_source = paste(sort(unique(name_source)), collapse = ", "),
      scientific_name = first(scientific_name),
      english_name = first(english_name),
      match_type = first(match_type),
      .groups = "drop"
    )
  
  matched_by_code_full <- unmatched_species %>%
    inner_join(matched_by_code_sources, by = "row_id")
  
  suggested_matches <- bind_rows(matched_by_name_full, matched_by_code_full) %>%
    distinct(row_id, code, name, scientific_name, english_name, match_type, matched_name_source)
  
  if (nrow(suggested_matches) > 0) {
    cat("\n🔍 Suggested additional matches:\n")
    print(suggested_matches %>% distinct(code, name, scientific_name, english_name, match_type, matched_name_source))
    
    confirm <- readline("Apply these additional matches? [y/n]: ")
    if (tolower(confirm) == "y") {
      phase2_full_rows <- data_nogeom %>%
        semi_join(suggested_matches, by = "row_id") %>%
        left_join(suggested_matches %>% select(row_id, scientific_name, english_name, match_type, matched_name_source), by = "row_id")
      
      phase2_full_rows$geometry <- geometry[phase2_full_rows$row_id]
      phase2_sf <- sf::st_as_sf(phase2_full_rows, sf_column_name = "geometry", crs = sf::st_crs(data))
      final_visits_sf <- bind_rows(matched_sf, phase2_sf)
      cat("✅ Additional matches applied.\n")
    } else {
      cat("❌ Additional matches skipped.\n")
      final_visits_sf <- matched_sf
    }
  } else {
    cat("ℹ️ No additional matches found.\n")
    final_visits_sf <- matched_sf
  }
  
  # 7. Final cleaning
  final_visits_sf <- final_visits_sf %>% select(-row_id)
  
  final_matched_codes <- final_visits_sf$code
  final_matched_names <- final_visits_sf$name
  
  still_unmatched_species <- data_nogeom[!(data_nogeom$code %in% final_matched_codes & data_nogeom$name %in% final_matched_names), ] %>%
    distinct(code, name)
  cat("❗ ", nrow(still_unmatched_species), " unmatched species remain.\n")
  View(still_unmatched_species)
  
  return(final_visits_sf)
}
