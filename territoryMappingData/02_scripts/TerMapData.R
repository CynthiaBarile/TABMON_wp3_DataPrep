## Territory mapping data - handling
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(googlesheets4)          # To read google sheets
library(janitor)
library(leaflet)
library(purrr)
library(readr)
library(sf)
library(tidyr)



## Import data
folder_path <- getwd()
shpfiles <- list.files(folder_path, pattern = ".shp", full.names = TRUE)

visits_sf <- st_read(shpfiles[1]) %>% st_transform(crs = 4326) # visits
areas_sf <- st_read(shpfiles[2]) %>% st_transform(crs = 4326) # areas
territories_sf <- st_read(shpfiles[3]) %>% st_transform(crs = 4326) # territories
transects_sf <- st_read(shpfiles[4]) %>% st_transform(crs = 4326) # tracks
overview_df <- read.csv("overview.csv") # overview

#############################################
## 🧹🧼 Clean, translate column names
#############################################

### Cleaning column and names
clean_data <- function(data, rename_map) {
  data <- data %>%
    rename_with(~ rename_map[.x], .cols = names(rename_map)[names(rename_map) %in% names(data)]) %>%
##    select(-any_of(drop_cols)) %>%   ## to drop columns if needed (add: drop_cols = NULL in function)
    clean_names()
  return(data)
}

## Define rename mappings and drop lists
rename_overview <- c(
  projectid = "project_id",
  plotid = "plot_id",
  naam = "site_name",
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

rename_areas <- c(
  plotid = "plot_id",
  projectid = "project_id",
  plotnummer = "plot_number",
  naam = "site_name",
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

# --- Apply cleaning functions ---
overview_df <- clean_data(overview_df, rename_overview)
areas_sf <- clean_data(areas_sf, rename_areas)
transects_sf <- clean_data(transects_sf, rename_transects)
visits_sf <- clean_data(visits_sf, rename_visits)
territories_sf <- clean_data(territories_sf, rename_territories)

#############################################
## 💡 Add translations
#############################################

species_key <- read.csv("../../Species_lists/Species_names_keys/final_multilingual_bird_key.csv")

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

# Call the function
mismatches_visits <- detect_species_mismatches(
  data = visits_sf, species_key = species_key,
  sp_code_col = "species_code",
  sp_name_col = "species_name",
  language = "dutch",
  return_unique_sp = TRUE)

## The inspection shows that most mismatches come from non-bird species, but not all
## So the filtering needs to be manual
non_birds <- c("Bruine Rat", "Damhert", "Konijn", "Ree", "Vos", "Haas", "Bever",
               "Edelhert", "Wild Zwijn", "Eekhoorn", "Muskusrat")

remove_non_bird <- function(data, species_name_col = "species_name", non_birds_list) {
  # Count how many records match the non-bird species list
  to_remove <- data %>%
    filter(!!sym(species_name_col) %in% non_birds_list)
  n_removed <- nrow(to_remove)
  # Filter
  birds_only <- data %>%
    filter(!(!!sym(species_name_col) %in% non_birds_list))
  # Print message:
  message(glue::glue("Removed {n_removed} non-bird records from source data."))
  # Return invisibly (output, but not showing it)
  return(invisible(birds_only))
}

visits_birds_only <- remove_non_bird(data = visits_sf, non_birds_list = non_birds)


################# Actual translation
# Implement the translation function
species_translation <- function(data, species_key,
                                sp_code_col = "species_code",
                                sp_name_col = "species_name",
                                language = "dutch") {
  
  library(dplyr)
  library(tidyr)
  library(sf)
  
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


visits_sf_en <- species_translation(data = visits_birds_only, species_key = species_key,
                    sp_code_col = "species_code",
                    sp_name_col = "species_name",
                    language = "dutch")

# warnings for many-to-many relationships, but normal
# 6 species unmatched

#############################################
## ️📌 Structure data by site
#############################################

# Lookup table from overview_df for correspondance between plot_id and site_name
plot_lookup <- overview_df %>% select(plot_id, site_name) %>% distinct()

# Get unique plot_ids
plot_ids <- unique(overview_df$plot_id)

# Group all relevant data by site
sites_data <- map(plot_ids, function(pid) {
  site_name <- plot_lookup %>%
    filter(plot_id == pid) %>%
    pull(site_name) %>%
    unique()
  
  list(
    plot_id = pid,
    site_name = site_name,
    overview = overview_df %>% filter(plot_id == pid),
    area = areas_sf %>% filter(plot_id == pid),
    transects = transects_sf %>% filter(plot_id == pid),
    visits = visits_sf_en %>% filter(plot_id == pid),
    territories = territories_sf %>% filter(plot_id == pid)
  )
})

# Name each list entry by plotID and site name
names(sites_data) <- sapply(sites_data, function(x) {
  clean_names <- gsub("[^a-zA-Z0-9]", "_", x$site_name)  # replace non-alphanumerics with _
  paste0(x$plot_id, "_", clean_names)                    # Paste plotID and site name, replace spaces with _
})


#############################################
## 🧬 Overlay with BUGGs
#############################################

# BUGG sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1Wqz7g0I2cqoMFSI-CkN3Fss1h62ceoos6_OtARi490c/edit?gid=208765239#gid=208765239"

# If it's the first time or not public, you may be prompted to authorize
sensor_metadata <- read_sheet(sheet_url, sheet = "Deployments details")

glimpse(sensor_metadata)     ## All good

# Convert to sf object using WGS84 (standard lon-lat CRS)
sensors_sf <- sensor_metadata %>%
  filter(Active == TRUE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Viz to then assign monitoring plots to BUGG clusters
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  
  addPolygons(data = areas_sf,
              color = "blue", weight = 2, fillOpacity = 0.2,
              popup = ~paste0(site_name, " (", plot_id, ")"),
              group = "Sovon Areas") %>%
  
  addCircleMarkers(data = sensors_sf,
                   radius = 5, fillColor = "red", fillOpacity = 0.9,
                   stroke = FALSE,
                   popup = ~paste0("Sensor: ", TABMONDeploymentID),
                   group = "Sensors") %>%
  
  addLayersControl(
    baseGroups = c("Satellite", "OSM"),
    overlayGroups = c("Sovon Areas", "Sensors"),
    options = layersControlOptions(collapsed = FALSE)
  )

## Match plot_id with corresponding cluster (defined for BUGGs)
plot_cluster_lookup <- tibble(
  plot_id = c(28568, 14885, 31748, 64770, 17364, 17081, 
              32459,
              39540, 19410,
              27443, 27441, 60201, 67176,
              25805, 17824, 23491, 39566, 18556, 14784, 23294),
  cluster = c(rep("Amsterdamse Waterleidingduinen", 6), 
              "Loenderveen",
              rep("Hoge Veluwe", 2),
              rep("Oostvaardersplassen", 4),
              rep("De Onlanden", 7))  # adjust as needed
)

# Merge cluster into areas
areas_sf <- areas_sf %>%
  left_join(plot_cluster_lookup, by = "plot_id")

# Propagate cluster info from areas to other datasets
transects_sf <- transects_sf %>%
  left_join(st_drop_geometry(areas_sf)[, c("plot_id", "cluster")], by = "plot_id")

visits_sf_en <- visits_sf_en %>%
  left_join(st_drop_geometry(areas_sf)[, c("plot_id", "cluster")], by = "plot_id")

territories_sf <- territories_sf %>%
  left_join(st_drop_geometry(areas_sf)[, c("plot_id", "cluster")], by = "plot_id")

overview_df <- overview_df %>%
  left_join(st_drop_geometry(areas_sf)[, c("plot_id", "cluster")], by = "plot_id")

# Store all to a list
datasets <- list(
  Overview = overview_df,
  Areas = areas_sf,
  Transects = transects_sf,
  Visits = visits_sf_en,           # Translated version of the visits data
  Territories = territories_sf,    # I didn't translate the territories yet because I don't need it
  Sensors = sensors_sf
)

saveRDS(datasets, file = "termap_bugg_metadata.rds")

## BELOW MAKES PROBABLY A BUNCH OF THINGS IN SUBSEQUENT SCRIPTS REDUNDANT
## Needs to be incorporated somewhere else earlier as well
## THIS ENTIRE PROJECT DIR NEEDS TO BE CLEANED UP ANYWAY

# BUT: below, creating one single Visits object containing likely all relevant info from other objects associated.
datasets <- readRDS("C:/Users/cbarile/My Drive/PostDoc UvA/TABMON/R_PROJECTS/termap_project/01_data/processed/termap_bugg_metadata.rds")

# --- Clean Overview ---
library(lubridate)
library(hms)
library(sf)

datasets$Overview <- datasets$Overview %>%
  mutate(
    date_parsed = dmy(paste(date, year)),           # e.g., "23-Mar 2025"
    start_time = as_hms(start_time),
    end_time = as_hms(end_time)
  )
# --- Clean Visits ---
datasets$Visits <- datasets$Visits %>%
  mutate(
    date = make_date(year, month, day)       # year/month/day -> Date
  )
saveRDS(datasets, file = "C:/Users/cbarile/My Drive/PostDoc UvA/TABMON/R_PROJECTS/termap_project/01_data/processed/termap_bugg_metadata.rds")


overview_join <- datasets$Overview %>%
  select(site_name, visit_id, start_time, end_time, visit_duration_minutes)

# join some columns from Overview to visits
st_geometry(datasets$Visits) <- "geometry"  # Somehow the geometry column was not registered anymore (attr(datasets$Visits, "sf_column") returned NULL) 
geom <- st_geometry(datasets$Visits)        # Extract the geometry column before the join

enriched_visits_data <- datasets$Visits %>%
  st_drop_geometry() %>%
  left_join(overview_join, by = "visit_id") %>%
  { st_geometry(.) <- geom; st_as_sf(.) }    # Reassigns the original geometry and re-set as sf object

saveRDS(enriched_visits_data, file = "enriched_termap_visits.rds")
