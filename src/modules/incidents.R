
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
                        name = "Meters\naffected",
                        trans = "log10") +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  coord_fixed(1.3,
              xlim = c(-125, -66),
              ylim = c(24, 50)) +
  labs(title = "Real-time power outages by county",
       subtitle = paste0("Rows: ", nrow(df)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())
        
}