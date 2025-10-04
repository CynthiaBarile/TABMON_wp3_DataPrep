### FOR BUGG EXPLO
# To plot deployment periods!

# Custom palette (same as static maps)
pal_custom <- c("#7F8864", "#CDB996", "#CFC97F", "#E69A7D", "#A94F5F")

plot_cluster_visits <- function(cluster_name, visits_data) {
  
  # Filter for one cluster
  cluster_visits <- visits_data %>%
    filter(cluster == cluster_name) %>%
    distinct(visit_id, .keep_all = TRUE) %>%
    group_by(date) %>%
    arrange(start_time) %>%
    mutate(visit_order = row_number()) %>%
    ungroup()
  
  # Create labels for dates (right side of lines)
  date_labels <- cluster_visits %>%
    group_by(date) %>%
    summarise(max_end = max(end_time), .groups = 'drop') %>%
    mutate(label_x = max_end + as_hms("00:20:00"))
  
  ggplot(cluster_visits, aes(y = date)) +
    geom_linerange(
      aes(xmin = start_time, xmax = end_time, color = factor(visit_order)),
      linewidth = 3, alpha = 0.6
    ) +
    geom_point(
      aes(x = start_time, color = factor(visit_order)),
      shape = 21, fill = "white", size = 3, stroke = 1
    ) +
    geom_point(
      aes(x = end_time, color = factor(visit_order)),
      size = 3
    ) +
    # geom_text(
    #   data = date_labels,
    #   aes(x = label_x, y = date, label = format(date, "%d %b")),
    #   inherit.aes = FALSE,
    #   hjust = 0, size = 3.5, color = "black"
    # ) +
    scale_x_time(
      breaks = scales::breaks_width("1 hour"),
      labels = scales::label_time("%H:%M"),
      limits = c(as_hms("03:45:00"), as_hms("23:59:00")),
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    scale_y_date(
      date_breaks = "2 weeks",
      date_labels = "%b",
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    # CUSTOM palette applied cyclically
    scale_color_manual(
      values = setNames(rep(pal_custom, 10), as.character(1:(length(pal_custom)*10))),
      name = "Visit # (per day)"
    ) +
    labs(
      title = paste("Survey dates & times —", cluster_name),
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
}

plot_cluster_visits("Loenderveen", visits_data)
plot_cluster_visits("Amsterdamse Waterleidingduinen", visits_data)
plot_cluster_visits("Hoge Veluwe", visits_data)
plot_cluster_visits("Oostvaardersplassen", visits_data)
plot_cluster_visits("De Onlanden", visits_data)

