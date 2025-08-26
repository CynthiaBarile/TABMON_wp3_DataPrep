## -----------------------------
## TABMON_dataprep.Rproject > territoryMappingData/
## 04_enrich_visits.R
## 💰 Enrich Visits with Overview info
## Input:  01_data > processed > 03_data_with_clusters.rds
## Output: 01_data > processed > 04_data_enriched.rds, 05_visits_only_data.rds
## -----------------------------

source("territoryMappingData/02_scripts/01_config.R")
data_with_clusters <- readRDS(data_with_clusters_path)    # Load clean, translated territory mapping data, with TABMON clusters info


# Extract `overview_df` and `visits_sf_en`
overview_df    <- data_with_clusters$overview_df
visits_sf_en   <- data_with_clusters$visits_sf_en

# Select columns from `overview_df` to join to `visits_sf_en`
overview_join <- overview_df %>%
  select(plot_name, visit_id, start_time, end_time, visit_duration_minutes)

# Enrich `visits_sf_en` with `overview_df` info
enriched_visits <- visits_sf_en %>%
  left_join(
    overview_df %>% select(visit_id, plot_name, start_time, end_time, visit_duration_minutes),
    by = "visit_id"
  )

# Optional: replace `visits_sf_en` in master list with updated, enriched version
data_with_clusters$visits_sf_en <- enriched_visits

# Save updated master territory mapping data bundle
saveRDS(data_with_clusters, file.path(data_proc, "04_data_enriched.rds"))

# Save enriched `visits_sf_en` object separately
saveRDS(enriched_visits, file.path(data_proc, "05_visits_only_data.rds"))