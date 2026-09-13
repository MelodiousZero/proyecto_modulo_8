library(dplyr)
library(ggplot2)
library(lubridate)


make_fuel_mix_per_day <- function(data,
                          fecha,
                          tz_salida = "America/Mexico_City"){
  
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
    "Geothermal"       = "#FFC0CB",
    "Wood"             = "#8B4513",
    "Waste"            = "#7F8C8D"
  )
  
  data <- data %>%
    rename_with(~ gsub("[.-]", "_", .x))
  
  fecha <- as.Date(fecha)
  
  df <- data %>%
    mutate(
      period = ymd_h(period) %>%
        force_tz(tzone = "UTC") %>%
        with_tz(tzone  = tz_salida),
      value  = as.numeric(value)
    ) %>%
    filter(as.Date(period, tz = tz_salida) == fecha)
  
  if (nrow(df) == 0) {
    stop("No hay datos para la fecha ", fecha,
         " en la zona horaria ", tz_salida)
  }
  
  day_data <- df %>%
    group_by(period, `type_name`) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  ggplot(day_data, aes(x = period, y = total_value, fill = `type_name`)) +
    geom_area(alpha = 0.9) +
    scale_fill_manual(values = fuel_colors) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_datetime(
      date_labels = "%Hh",
      date_breaks = "3 hours",
      timezone    = tz_salida
    ) +
    labs(
      title = paste0("Fuel Mix — ", format(fecha, "%Y-%m-%d")),
      x = "Hora",
      y = "Generación (MWh)",
      fill = "Tipo de energía"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}