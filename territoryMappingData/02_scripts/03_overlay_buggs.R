## -----------------------------
## TABMON_dataprep.Rproject, subfolder territoryMappingData
## 03_overlay_buggs.R
## ⬆️ Loading BUGG metadata
## 🧼 Cleaning
## 💰 Enrich data
## Input:  01_data > processed > 02_translated_data.rds + BUGG sensors metadata
## Output: 01_data > processed > 03_data_with_clusters.rds
## -----------------------------

source("02_scripts/01_config.R")                                              # Load config
translated_data <- readRDS(translated_data_path)                              # Load clean, translated territory mapping data
sensor_metadata <- read_sheet(bugg_sheet_url, sheet = "Deployments details")  # Read BUGG metadata sheet (stored at bugg_sheet_url)

glimpse(sensor_metadata)                                                      # Check it

# Convert to sf object using WGS84 (standard lon-lat CRS)
sensors_sf <- sensor_metadata %>%
  filter(Active == TRUE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Viz to then match monitoring plots with nearby BUGGs
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addPolygons(data = translated_data$plots_sf,
              color = "blue", weight = 2, fillOpacity = 0.2,
              popup = ~paste0(plot_name, " (", plot_id, ")"),
              group = "Territory mapping plots") %>%
  addCircleMarkers(data = sensors_sf,
                   radius = 5, fillColor = "red", fillOpacity = 0.9,
                   stroke = FALSE,
                   popup = ~paste0("Sensor: ", TABMONDeploymentID),
                   group = "Sensors") %>%
  addLayersControl(
    baseGroups = c("Satellite", "OSM"),
    overlayGroups = c("Territory mapping plots", "Sensors"),
    options = layersControlOptions(collapsed = FALSE)
  )

## Match `plot_id` with corresponding cluster (defined for BUGGs)
plot_cluster_lookup <- tibble(
  plot_id = c(28568, 14885, 31748, 64770, 17364, 17081,           # AWD monitoring plots
              32459,                                              # Loenderveen
              39540, 19410,                                       # Hoge Veluwe
              27443, 27441, 60201, 67176,                         # Oostvaardersplassen
              25805, 17824, 23491, 39566, 18556, 14784, 23294),   # De Onlanden
  cluster = c(rep("Amsterdamse Waterleidingduinen", 6), 
              "Loenderveen",
              rep("Hoge Veluwe", 2),
              rep("Oostvaardersplassen", 4),
              rep("De Onlanden", 7))
)

# Add cluster (TABMON cluster names) into `plots_sf` from `translated_data`
translated_data$plots_sf <- translated_data$plots_sf %>%
  left_join(plot_cluster_lookup, by = "plot_id")

# Propagate cluster info to other datasets in the `translated_data` list
for(name in c("transects_sf", "visits_sf_en", "territories_sf", "overview_df")) {
  translated_data[[name]] <- translated_data[[name]] %>%
    left_join(st_drop_geometry(translated_data$plots_sf)[, c("plot_id", "cluster")], by = "plot_id")
}

# Save territory mapping data now enriched with BUGG clusters information
saveRDS(translated_data, file = file.path(data_proc, "03_data_with_clusters.rds"))
