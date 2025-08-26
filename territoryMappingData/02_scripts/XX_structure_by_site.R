# NO LONGER USED, NEEDS MODIFIED IF NEEDED! 
# PERHAPS TO BE USED ON THE VISITS OBJECT ONLY WHEN STARTING COMPARISONS

## -----------------------------
## TABMON_dataprep.Rproject > territoryMappingData/
## XX_structure_by_site.R
## ⬆️ Loading data sources: 01_data > processed > 04_data_enriched
## 📍 Structure data by site
## -----------------------------

source("territoryMappingData/02_scripts/01_config.R")
data_enriched <- readRDS(data_enriched_path)           # Load clean, translated territory mapping data, with TABMON clusters info and enriched visits data

#############################################
## --- Structure data by site ---
#############################################

# Lookup table from overview_df for correspondence between plot_id and plot_name
plot_lookup <- data_enriched$overview_df %>% 
  select(plot_id, plot_name) %>% 
  distinct()

# Helper to bundle all datasets for a single plot_id
bundle_site_data <- function(plot_id, plot_name) {
  list(
    plot_id     = plot_id,
    plot_name   = plot_name,
    overview    = filter(data_enriched$overview_df, plot_id == plot_id),
    plots       = filter(data_enriched$plots_sf, plot_id == plot_id),
    transects   = filter(data_enriched$transects_sf, plot_id == plot_id),
    visits      = filter(data_enriched$visits_sf_en, plot_id == plot_id),
    territories = filter(data_enriched$territories_sf, plot_id == plot_id)
  )
}

# Build sites_data in one go
sites_data <- pmap(plot_lookup, bundle_site_data)

# Assign monitoring plot names for convenience (e.g. "28568_Amsterdamse Waterleidingduinen")
names(sites_data) <- sapply(sites_data, function(x) {
  paste0(x$plot_id, "_", x$plot_name)
})

# Save object: data bundled by monitoring plot
saveRDS(sites_data, file.path(data_proc, "XX_data_by_site.rds"))