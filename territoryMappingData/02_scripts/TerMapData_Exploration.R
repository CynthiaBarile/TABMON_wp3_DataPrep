## Territory Mapping Data exploration

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Packages
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hms)

## Data loading
termap_data <- readRDS("terrmap_bugg_metadata.rds")

#############################################
## ️🪟 Visualisation
#############################################

# Function to build cluster map

build_cluster_map <- function(cluster_name, datasets) {
  pal <- brewer.pal(5, "Set2")
  
  # Filter sensor points for the cluster
  sensors_cluster <- datasets$Sensors %>% filter(cluster == cluster_name)
  # Project sensors to metric CRS for buffering (EPSG:3857)
  sensors_proj <- st_transform(sensors_cluster, 3857)
  buffer_50m <- st_buffer(sensors_proj, dist = 50) %>% st_transform(4326)
  buffer_100m <- st_buffer(sensors_proj, dist = 100) %>% st_transform(4326)
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(
      data = datasets$Areas %>% filter(cluster == cluster_name),
      color = pal[1], weight = 1, fillOpacity = 0.4,
      group = "Areas"
    ) %>%
    
    addPolylines(
      data = datasets$Transects %>% filter(cluster == cluster_name),
      color = pal[2], weight = 2,
      group = "Transects"
    ) %>%
    
    # Visits with popup
    addCircleMarkers(
      data = datasets$Visits %>% filter(cluster == cluster_name),
      radius = 2, color = pal[3], fillOpacity = 0.3, group = "Visits",
      popup = ~paste0(
        "<b>Species:</b> ", english_name, "<br>",
        "<b>Date:</b> ", paste(year, month, day, sep = "-"), "<br>"
      )
    ) %>%
    
    addCircleMarkers(
      data = datasets$Territories %>% filter(cluster == cluster_name),
      color = pal[4], radius = 1, fillOpacity = 0.4,
      group = "Territories"
    ) %>%
    
    addPolygons(
      data = buffer_100m,
      fillColor = pal[5],
      fillOpacity = 0.2,
      stroke = FALSE,
      group = "Sensors: 100m buffer"
    ) %>%
    
    addPolygons(
      data = buffer_50m,
      fillColor = pal[5],
      fillOpacity = 0.3,
      stroke = FALSE,
      group = "Sensors: 50m buffer"
    ) %>%
    
    addCircleMarkers(
      data = sensors_cluster,
      color = pal[5],
      radius = 6,
      fillOpacity = 1,
      stroke = FALSE,
      popup = ~as.character(TABMONDeploymentID),
      group = "Sensors"
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Areas", "Transects", "Visits", "Territories", 
                        "Sensors: 50m buffer", "Sensors: 100m buffer", "Sensors"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
}

map_AWD <- build_cluster_map("Amsterdamse Waterleidingduinen", termap_data)
map_LDV <- build_cluster_map("Loenderveen", termap_data)
map_OVP <- build_cluster_map("Oostvaardersplassen", termap_data)
map_HV <- build_cluster_map("Hoge Veluwe", termap_data)
map_OL <- build_cluster_map("De Onlanden", termap_data)

#############################################
## 👟 Exploration
#############################################
#### PART 1: Survey info
# Work with Loenderveen

ldv_termap_data <- list(
  Overview    = filter(termap_data$Overview, cluster == "Loenderveen"),
  Areas       = filter(termap_data$Areas, cluster == "Loenderveen"),
  Transects   = filter(termap_data$Transects, cluster == "Loenderveen"),
  Visits      = filter(termap_data$Visits, cluster == "Loenderveen"),
  Territories = filter(termap_data$Territories, cluster == "Loenderveen"),
  Sensors     = filter(termap_data$Sensors, cluster == "Loenderveen")
)

# Summary of visits
summary_stats <- ldv_termap_data$Overview %>%
  summarise(
    n_visits = n_distinct(visit_id),
    n_days = n_distinct(date),
    n_observers = n_distinct(observer_id),
    total_duration_h = sum(visit_duration_minutes, na.rm = TRUE) / 60,
    average_distinct_species = mean(n_species, na.rm = TRUE),
    total_records = sum(n_records, na.rm = TRUE)
  )
print(summary_stats)

ldv_overview <- ldv_termap_data$Overview %>%
  mutate(
    date_parsed = dmy(paste(date, year)),
    start = as_hms(start_time),
    end = as_hms(end_time)
  ) %>%
  group_by(date_parsed) %>%
  mutate(visit_order = row_number()) %>%  # visit number per day
  ungroup()

# Create a distinct dates data frame for labeling
# Find the rightmost 'end' time per date for placing the label
date_labels <- ldv_overview %>%
  group_by(date_parsed) %>%
  summarise(
    max_end = max(end),
    .groups = 'drop'
  ) %>%
  mutate(label_x = max_end + as_hms("00:20:00"))  # shift label 5 minutes to the right

ggplot(ldv_overview, aes(y = date_parsed)) +
  geom_linerange(
    aes(xmin = start, xmax = end, color = factor(visit_order)),
    linewidth = 3, alpha = 0.6
  ) +
  geom_point(
    aes(x = start, color = factor(visit_order)),
    shape = 21, fill = "white", size = 3, stroke = 1
  ) +
  geom_point(
    aes(x = end, color = factor(visit_order)),
    size = 3
  ) +
  geom_text(
    data = date_labels,
    aes(x = label_x, y = date_parsed, label = format(date_parsed, "%d %b")),  # no year
    inherit.aes = FALSE,
    hjust = 0, size = 3.5, color = "black"
  ) +
  scale_x_time(
    breaks = breaks_width("1 hour"),
    labels = label_time("%H:%M"),
    limits = c(as_hms("04:45:00"), as_hms("23:45:00")),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_color_brewer(palette = "Set2", name = "Visit #") +
  scale_y_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Survey dates & times — Loenderveen",
    x = "Time of Day",
    y = "Month"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey80"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 15, face = "bold", margin = margin(b = 10))
  )

### PART 2: Species compo explo
library(dplyr)
library(tidyr)
library(ggplot2)

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