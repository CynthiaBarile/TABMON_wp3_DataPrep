## -----------------------------
## TABMON_dataprep.Rproject > territoryMappingData/
## data_visualisation.R
## 📊 Visualisation of territory mapping data
## Input:  01_data > processed > 04_data_enriched
## Output: 03_outputs
## -----------------------------

# --- Configuration & libraries ---
source("territoryMappingData/02_scripts/01_config.R")

library(colorspace)
library(dplyr)
library(ggspatial)
library(googlesheets4)
library(ggplot2)
library(leaflet)
library(lubridate)
library(scales)  # for gradient_n_pal
library(sf)

crs_projected <- 28992     # Projected local CRS Amersfoort / RD New
buffer100 <- 100           # Buffer radius in meters

# --- Load enriched data ---
termap_data <- readRDS(data_enriched_path)
visits_data <- readRDS(visits_data_path)

# --- Access BUGGs metadata ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1Wqz7g0I2cqoMFSI-CkN3Fss1h62ceoos6_OtARi490c/edit?gid=208765239#gid=208765239"
sensor_metadata <- read_sheet(sheet_url, sheet = "Deployments details")

# Convert to sf object (WGS84 lon-lat)
sensors_sf <- sensor_metadata %>%
  filter(Active == TRUE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# =====================================
# --- General stats ---
# =====================================
# Summary of visits
summary_stats <- termap_data$overview_df %>%
  summarise(
    n_visits = n_distinct(visit_id),
    n_days = n_distinct(date),
    n_observers = n_distinct(observer_id),
    total_duration_h = sum(visit_duration_minutes, na.rm = TRUE) / 60,
    average_distinct_species = mean(n_species, na.rm = TRUE),
    total_records = sum(n_records, na.rm = TRUE)
  )
print(summary_stats)

# Date ranges of visits per cluster
cluster_summary <- termap_data$overview_df %>%
  group_by(cluster) %>%
  summarise(n_visits = n(),
            season_start = min(date, na.rm = TRUE),
            season_end   = max(date, na.rm = TRUE))

cluster_summary

# =====================================
# --- Dynamic Leaflet Map per cluster ---
# =====================================
# --- Palette for dynamic maps (leaflet) ---
pal_leaflet <- list(
  "Plots" = "#7F8864",
  "Transects" = "#CDC37A",
  "Observations" = "#CAB691",
  "Territories" = "#E69A7D",
  "Sensors" = "#BB8588")

# --- Dynamic Leaflet Map ---
dynamic_cluster_map <- function(cluster_name, datasets) {
  # Filter sensor points
  sensors_cluster <- sensors_sf %>% filter(cluster == cluster_name)
  # 100m buffer
  sensors_proj <- st_transform(sensors_cluster, 3857)
  buffer_100m <- st_buffer(sensors_proj, dist = buffer100) %>% st_transform(4326)
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Plots
    addPolygons(
      data = datasets$plots_sf %>% filter(cluster == cluster_name),
      color = pal_leaflet[["Plots"]], weight = 1, fillOpacity = 0.4,
      group = "Plots") %>%
    # Transects
    addPolylines(
      data = datasets$transects_sf %>% filter(cluster == cluster_name),
      color = pal_leaflet[["Transects"]], weight = 2,
      group = "Transects") %>%
    # Visits
    addCircleMarkers(
      data = datasets$visits_sf_en %>% filter(cluster == cluster_name),
      radius = 2, color = pal_leaflet[["Observations"]], fillOpacity = 0.3, group = "Observations",
      popup = ~paste0("<b>Species:</b> ", english_name, "<br>",
                      "<b>Date:</b> ", date)) %>%
    # Territories
    addCircleMarkers(
      data = datasets$territories_sf %>% filter(cluster == cluster_name),
      color = pal_leaflet[["Territories"]], radius = 1, fillOpacity = 0.4,
      group = "Territories") %>%
    # Sensor buffer
    addPolygons(
      data = buffer_100m,
      fillColor = pal_leaflet[["Sensors"]], fillOpacity = 0.5, stroke = FALSE,
      group = "Sensors: 100m buffer") %>%
    # Sensors
    addCircleMarkers(
      data = sensors_cluster,
      color = pal_leaflet[["Sensors"]], radius = 6, fillOpacity = 1, stroke = FALSE,
      popup = ~as.character(TABMONDeploymentID),
      group = "Sensors") %>%
    addLayersControl(
      overlayGroups = c("Plots", "Transects", "Observations", "Territories",
                        "Sensors: 100m buffer", "Sensors"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
}

# Dynamic maps
dynamic_cluster_map("Amsterdamse Waterleidingduinen", termap_data)
dynamic_cluster_map("Loenderveen", termap_data)
dynamic_cluster_map("Oostvaardersplassen", termap_data)
dynamic_cluster_map("Hoge Veluwe", termap_data)
dynamic_cluster_map("De Onlanden", termap_data)


# =====================================
# --- Static ggplot Map per cluster ---
# =====================================
pal_static <- c("Plots" = "#7F8864",
                "Transects" = "#CDB996",
                "Sensors" = "#BB8588",
                "Sensor 100m buffer" = "#BB8588")

static_cluster_map <- function(cluster_name, datasets) {
  
  # Filter data
  plots_cluster <- datasets$plots_sf %>% filter(cluster == cluster_name)
  transects_cluster <- datasets$transects_sf %>% filter(cluster == cluster_name)
  sensors_cluster <- sensors_sf %>% filter(cluster == cluster_name)
  
  # Add layer type for legend
  plots_cluster$layer <- "Plots"
  transects_cluster$layer <- "Transects"
  sensors_cluster$layer <- "Sensors"
  
  # 100m buffer
  sensors_proj <- st_transform(sensors_cluster, 3857)
  buffer_100m <- st_buffer(sensors_proj, dist = 100) %>% st_transform(4326)
  buffer_100m$layer <- "Sensor 100m buffer"
  
  ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 15) +
    geom_sf(data = plots_cluster, aes(fill = layer), color = NA, alpha = 0.5) +
    geom_sf(data = buffer_100m, aes(fill = layer), color = NA, alpha = 0.3) +  # alpha here
    geom_sf(data = transects_cluster, aes(color = layer), size = 1) +
    geom_sf(data = sensors_cluster, aes(color = layer), size = 3) +
    annotation_scale(location = "br", width_hint = 0.2, style = "ticks") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      legend.position = "right",
      legend.key = element_blank()) +
    # Remove override.aes for fill → legend uses the actual alpha from geoms
    guides(
      fill = guide_legend(title = NULL),
      color = guide_legend(title = NULL)) +
    scale_fill_manual(values = pal_static) +
    scale_color_manual(values = pal_static) +
    labs(title = cluster_name)
}

# Static maps
static_cluster_map("Amsterdamse Waterleidingduinen", termap_data)
static_cluster_map("Loenderveen", termap_data)
static_cluster_map("Oostvaardersplassen", termap_data)
static_cluster_map("Hoge Veluwe", termap_data)
static_cluster_map("De Onlanden", termap_data)

# =====================================
## --- Heatmaps survey effort ---
# =====================================

# Heatmap palette: white for 0, warm accent colors for increasing effort
# Earthy tones for gradient
pal_earthy <- c("#FFFFFF", "#E6D8B0", "#A3B18A", "#5C6B49")

plot_cluster_effort <- function(cluster_name, interval_minutes = 30, n_colors = 100, show_legend = TRUE) {
  # Filter cluster data
  df <- termap_data$overview_df %>%
    filter(cluster == cluster_name) %>%
    mutate(start_dt = as.POSIXct(start_datetime_local),
           end_dt   = as.POSIXct(end_datetime_local)) %>%
    filter(!is.na(start_dt) & !is.na(end_dt))
  
  if (nrow(df) == 0) stop("No data for this cluster")
  
  # Create interval grid
  min_time <- floor_date(min(df$start_dt), "day")
  max_time <- ceiling_date(max(df$end_dt), "day")
  
  intervals <- data.frame(
    interval_start = seq(min_time, max_time, by = paste0(interval_minutes, " mins"))
  )
  intervals$interval_end <- intervals$interval_start + minutes(interval_minutes)
  intervals$date <- as.Date(intervals$interval_start)
  intervals$time_of_day <- format(intervals$interval_start, "%H:%M")
  intervals$minutes_surveyed <- 0
  
  # Sum minutes per interval
  for (i in seq_len(nrow(df))) {
    overlap_start <- pmax(df$start_dt[i], intervals$interval_start)
    overlap_end   <- pmin(df$end_dt[i], intervals$interval_end)
    overlap_minutes <- as.numeric(difftime(overlap_end, overlap_start, units = "mins"))
    overlap_minutes[overlap_minutes < 0] <- 0
    intervals$minutes_surveyed <- intervals$minutes_surveyed + overlap_minutes
  }
  
  # Earthy palette: white = 0, then your palette for non-zero effort
  gradient_colors <- c("white", colorRampPalette(pal_earthy, space = "Lab")(n_colors))
  
  # Y-axis breaks: always 1st + 15th of each month
  date_seq <- seq(min(intervals$date), max(intervals$date), by = "1 day")
  y_breaks <- date_seq[day(date_seq) %in% c(1, 10, 20)]
  
  # Plot heatmap
  p <- ggplot(intervals, aes(x = time_of_day, y = date, fill = minutes_surveyed)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colors = gradient_colors, 
                         na.value = "white",
                         limits = c(0, 90),
                         oob = scales::squish,
                         breaks = c(0, 30, 60, 90)) +
    scale_x_discrete(breaks = unique(intervals$time_of_day)[seq(1, length(unique(intervals$time_of_day)), 2)]) + # hourly labels
    scale_y_date(breaks = y_breaks, date_labels = "%b %d") +
    labs(x = "Time of day",
         y = "Date",
         fill = "Minutes surveyed",
         title = paste(cluster_name)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          panel.grid = element_blank())
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  
  return(p)
}

# Example usage:
plot_cluster_effort("De Onlanden", show_legend = FALSE)
plot_cluster_effort("Loenderveen", show_legend = FALSE)
plot_cluster_effort("Amsterdamse Waterleidingduinen", show_legend = FALSE)
plot_cluster_effort("Hoge Veluwe", show_legend = FALSE)
plot_cluster_effort("Oostvaardersplassen", show_legend = FALSE)





### random things from before
# To re-shuffle completely if it's of interest

### PART 2: Species compo explo

# Drop geometry & adds properly formatted date & start/end times of visits from ldv_overview
visits_df <- ldv_termap_data$Visits %>%
  st_drop_geometry() %>%
  select(visit_id, english_name, count) %>%
  left_join(ldv_overview %>%
              select(visit_id, date_parsed, start, end, visit_duration_minutes), by = "visit_id") %>%
  mutate(count_per_hr = count / (visit_duration_minutes / 60))

# Heatmap species-by-date
# Prepare data for heatmap
# Step 1: Lump species first
visits_lumped <- visits_df %>%
  mutate(
    english_name = fct_lump_min(english_name, min = 10),
    english_name = as.character(english_name)
  )

# Step 2: Summarise by date and species (now properly lumped)
heatmap_df <- visits_lumped %>%
  group_by(date_parsed, english_name) %>%
  summarise(total_per_hr = sum(count_per_hr, na.rm = TRUE), .groups = "drop")

# Step 3: Pivot wider — now safe
heatmap_matrix <- pivot_wider(
  heatmap_df,
  names_from = english_name,
  values_from = total_per_hr,
  values_fill = list(total_per_hr = 0)
)

# Convert to long format again for ggplot
heatmap_long <- heatmap_matrix %>%
  pivot_longer(-date_parsed, names_to = "species", values_to = "count_per_hr")

# Plot
ggplot(heatmap_long, aes(x = species, y = date_parsed, fill = count_per_hr)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", na.value = "grey90") +
  labs(
    title = "Species Composition (standardized by effort)",
    x = "Species",
    y = "Survey Date",
    fill = "Count/hr"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Stacked barplot
library(colorspace)
# After lumping into Other species with less than 10 records, 19 sp left + other
pal1 <- brewer.pal(12,"Set3")
pal2 <- brewer.pal(8,"Set2")
pal <- c(pal1, pal2)[1:20]

ggplot(heatmap_long, aes(x = date_parsed, y = count_per_hr, fill = species)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal) +
  labs(x = "Survey date", y = "Counts per hour", fill = "Species") +
  theme_minimal()


# Top 5 per survey
top5_by_date <- visits_df %>%
  group_by(date_parsed, english_name) %>%
  summarise(total_per_hr = sum(count_per_hr, na.rm = TRUE), .groups = "drop") %>%
  group_by(date_parsed) %>%
  slice_max(total_per_hr, n = 5, with_ties = FALSE) %>%
  ungroup()


ggplot(top5_by_date, aes(x = english_name, y = total_per_hr, fill = english_name)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ date_parsed, scales = "free_x") +  # free x-axis to remove empty bars
  scale_fill_brewer(palette = "Set3", name = "Species") +
  labs(
    x = NULL,  # hide x-axis label
    y = "Counts per Hour",
    title = "Top 5 Species per Survey Date"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),  # hide species names on x-axis
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

