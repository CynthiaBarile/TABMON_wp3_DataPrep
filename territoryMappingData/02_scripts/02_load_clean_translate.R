## -----------------------------
## TABMON_dataprep.Rproject > territoryMappingData/
## 02_load_clean_translate.R
## ⬆️ Loading data sources
## 🧼 Cleaning data (standardising, deleting redundant columns, handling dates and times)
## 🗣️ Translations
## Input:  01_data > raw > all shapefiles + overview csv / reference bird list from separate directory
## Output: 01_data > processed > 02_translated_data.rds
## -----------------------------

source("territoryMappingData/02_scripts/01_config.R")

#############################################
## --- Import data ---
#############################################
list.files(data_raw, full.names = TRUE)

visits_sf <- st_read(file.path(data_raw, "bezoekpunten.shp")) %>% st_transform(crs = 4326)          # visits
plots_sf <- st_read(file.path(data_raw, "gebieden.shp")) %>% st_transform(crs = 4326)               # areas
territories_sf <- st_read(file.path(data_raw, "territoria.shp")) %>% st_transform(crs = 4326)       # territories
transects_sf <- st_read(file.path(data_raw, "tracks.shp")) %>% st_transform(crs = 4326)             # tracks
overview_df <- read.csv(file.path(data_raw, "overview.csv"))                                        # overview

#############################################
# --- Rename, format date & time, clean ---
#############################################
# /!\ Saw at later stages, that one visit (visit_id == 4107909) does not appear in the visits_sf because no bird seen
# This is still effort, so should be kept. 

overview_df <- clean_data(overview_df, 
                          rename_map = rename_overview, 
                          clean_name_cols = "plot_name") %>%
  mutate(date                 = dmy(paste(date, year)),                   # consolidate date, year
         start_time           = as_hms(start_time),                       # format times appropriately
         end_time             = as_hms(end_time),
         start_datetime_local = force_tz(as_datetime(date) + seconds(start_time), tzone = "Europe/Amsterdam"),
         end_datetime_local   = force_tz(as_datetime(date) + seconds(end_time), tzone = "Europe/Amsterdam"),
         start_datetime_utc   = with_tz(start_datetime_local, "UTC"),
         end_datetime_utc     = with_tz(end_datetime_local,   "UTC")) %>%
  select(-year, -day_of_year, -project_id)

visits_sf <- clean_data(visits_sf, 
                        rename_map = rename_visits) %>%
  mutate(date = make_date(year, month, day)) %>%                 # consolidate year/month/day
  relocate(date, .before = day) %>%                              # move that new column
  select(-year, -month, -day, -day_of_year, -project_id)         # remove those now redundant columns

# >>> Add missing visit from overview_df into visits_sf
visit_missing <- overview_df %>%
  filter(visit_id == 4107909) %>%
  select(any_of(names(visits_sf))) # done

transects_sf <- clean_data(transects_sf, 
                           rename_map = rename_transects, 
                           drop_cols = c("year", "project_id"))

territories_sf <- clean_data(territories_sf, 
                             rename_map = rename_territories, 
                             clean_name_cols = "plot_name", 
                             drop_cols = c("year", "project_id"))

plots_sf <- clean_data(plots_sf,
                       rename_map = rename_plots, 
                       clean_name_cols = "plot_name", 
                       drop_cols = "project_id")

# Save those objects
saveRDS(list(
  transects_sf   = transects_sf,
  plots_sf       = plots_sf,
  territories_sf = territories_sf,
  visits_sf      = visits_sf,
  overview_df    = overview_df),
  file = file.path(data_proc, "01_cleaned_data.rds"))

#############################################
# --- Handling species in Visits ---
# Delete non-bird, translate
#############################################

# First, clean-up non-bird species
# Call the function to identify mismatches (comparing with European bird species key)
mismatches_visits <- detect_species_mismatches(
  data = visits_sf, species_key = species_key,
  sp_code_col = "species_code",
  sp_name_col = "species_name",
  language = "dutch",
  return_unique_sp = TRUE)

## The inspection shows that most mismatches come from non-bird species, but not all
# enter below list of species that need to be excluded (capitals or no capitals, not a problem, the comparison is coded to be case-insensitive)
non_birds <- c("bruine rat", "damhert", "konijn", "ree", "vos", "haas", "bever",
               "edelhert", "wild zwijn", "eekhoorn", "muskusrat")

# Remove those species
visits_birds_only <- remove_non_bird(data = visits_sf, non_birds_list = non_birds)

# Translate
visits_sf_en <- species_translation(data = visits_birds_only, species_key = species_key,
                                    sp_code_col = "species_code",
                                    sp_name_col = "species_name",
                                    language = "dutch")

# warnings for many-to-many relationships, but normal
# 6 species unmatched (barmsijs (grote of kleine), witsterblauwborst, soepeend, witkeelkwikstaart, rouwkwikstaart, stadsduif)

# Save intermediate object: same as 01_cleaned_data.rds, but with translated visits_sf_en
saveRDS(list(
  transects_sf   = transects_sf,
  plots_sf       = plots_sf,
  territories_sf = territories_sf,
  visits_sf_en   = visits_sf_en,
  overview_df    = overview_df),
  file = file.path(data_proc, "02_translated_data.rds"))
