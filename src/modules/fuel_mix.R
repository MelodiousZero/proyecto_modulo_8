

library(dplyr)
library(ggplot2)
library(lubridate)


make_fuel_mix <- function(data){

  fuel_colors <- c(
    "Coal"             = "#4d4d4d",
    "Natural Gas"      = "#FF8C00",
    "Nuclear"          = "#9B59B6",
    "Hydro"            = "#3498DB",
    "Wind"             = "#2ECC71",
    "Solar"            = "#F1C40F",
    "Battery storage"  = "#1ABC9C",
    "Other"            = "#95A5A6",
    "Petroleum"        = "#E74C3C",
    "Unknown"          = "#BDC3C7",
    "Geothermal"       = "#E67E22",
    "Wood"             = "#8B4513",
    "Waste"            = "#7F8C8D"
  )
  
  df <- data %>%
    mutate(
      datetime = ymd_h(period),  # Parse "2026-09-10T07" correctly
      value = as.numeric(value)
    )
  
  # Get the most recent day's data
  latest_day <- max(ymd_h(df$period))
  day_data <- df %>%
    mutate(period = ymd_h(period)) %>%
    filter(date(period) == date(latest_day)) %>%
    group_by(period, `type_name`) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Stacked area chart for one day
  ggplot(day_data, aes(x = period, y = total_value, fill = `type_name`)) +
    geom_area(alpha = 0.9) +
    scale_fill_manual(values = fuel_colors) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
    labs(
      title = paste("Fuel Mix on", format(latest_day, "%B %d, %Y")),
      x = "Hour",
      y = "Generation (MWh)",
      fill = "Fuel Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}