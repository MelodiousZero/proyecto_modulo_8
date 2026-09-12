
library(sf)
library(ggplot2)
library(maps)
library(dplyr)


make_incidents_map <- function(df){
    
# Coerce numeric columns (read.csv may have made them character)
df <- df %>%
  mutate(
    metersaffected = as.numeric(metersaffected),
    centroid.lon   = as.numeric(centroid.lon),
    centroid.lat   = as.numeric(centroid.lat)
  ) %>%
  filter(!is.na(centroid.lon), !is.na(centroid.lat))

# ---- US basemap ----
us_states <- map_data("state")

# ---- Plot ----
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_point(data = df,
             aes(x = centroid.lon, y = centroid.lat,
                 size = metersaffected, color = metersaffected),
             alpha = 0.7) +
  scale_color_viridis_c(option = "inferno",
                        name = "Medidores\nafectados",
                        trans = "log10") +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  coord_fixed(1.3,
              xlim = c(-125, -66),
              ylim = c(24, 50)) +
  labs(title = "Apagones",
       subtitle = paste0("Condados afectados: ", nrow(df)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())
        
}




# ---------------------------------------------------------------
# Bar graph: outage status (statuskind)
# ---------------------------------------------------------------
make_status_bar_graph <- function(df) {
  
  plot_df <- df %>%
    mutate(statuskind = trimws(as.character(statuskind))) %>%
    filter(!is.na(statuskind),
           statuskind != "",
           !statuskind %in% c("NA", "null", "NULL", "N/A")) %>%
    count(statuskind, sort = TRUE) %>%
    rename(status = statuskind, n = n)
  
  if (nrow(plot_df) == 0) {
    return(plotly_empty(type = "bar") %>%
             layout(title = "No status data available"))
  }
  
  p <- ggplot(plot_df,
              aes(x = reorder(status, n), y = n,
                  fill = status,
                  text = paste0(status, ": ", n, " incidents"))) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.15, size = 3.5) +
    coord_flip() +
    scale_fill_viridis_d(option = "viridis", direction = -1) +
    labs(title = "Status",
         x = NULL, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank())
  
  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 120))
}


# ---------------------------------------------------------------
# Bar graph: outage causes (causekind with fallback to cause)
# ---------------------------------------------------------------
make_causes_bar_graph <- function(df) {
  
  # Prefer `causekind` (standardised), fall back to `cause` (free text)
  pick_col <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | x == "" | x %in% c("NA", "null", "NULL", "N/A")] <- NA
    x
  }
  
  plot_df <- df %>%
    mutate(
      causekind_clean = pick_col(causekind),
      cause_clean     = pick_col(cause)
    ) %>%
    mutate(cause_final = coalesce(causekind_clean, cause_clean)) %>%
    filter(!is.na(cause_final)) %>%
    count(cause_final, sort = TRUE) %>%
    rename(cause = cause_final, n = n)
  
  if (nrow(plot_df) == 0) {
    return(plotly_empty(type = "bar") %>%
             layout(title = "No cause data available"))
  }
  
  # Truncate very long cause labels so the chart stays readable
  plot_df <- plot_df %>%
    mutate(cause_label = ifelse(nchar(cause) > 40,
                                paste0(substr(cause, 1, 37), "..."),
                                cause))
  
  p <- ggplot(plot_df,
              aes(x = reorder(cause_label, n), y = n,
                  fill = cause_label,
                  text = paste0(cause, ": ", n, " incidents"))) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.15, size = 3.5) +
    coord_flip() +
    scale_fill_viridis_d(option = "plasma", direction = -1) +
    labs(title = "Causa",
         x = NULL, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank())
  
  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 150))
}