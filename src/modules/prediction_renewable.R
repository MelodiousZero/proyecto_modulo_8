library(openmeteo)
library(dplyr)
library(tidyverse)
library(lubridate)
library(xgboost)

make_prediction_renewables <- function(data_generation_by_energy_source,
                                       target_respondent = "CISO",
                                       model_dir = "modules/machine_learning/models",
                                       forecast_days = 3,
                                       past_days = 4) {
  
 
  RENEWABLE_FUELS <- c("GEO", "SNB", "SUN", "WAT", "WND")
  
  FUEL_LABELS <- c(
    SUN = "Solar", SNB = "Solar + Battery", WND = "Wind",
    WAT = "Hydro", GEO = "Geothermal"
  )
  
  FEATURES <- list(
    SUN = c("shortwave_radiation", "direct_normal_irradiance",
            "diffuse_radiation", "cloud_cover", "cloud_cover_low",
            "cloud_cover_mid", "temperature_2m",
            "hour_sin", "hour_cos", "doy_sin", "doy_cos"),
    SNB = c("shortwave_radiation", "direct_normal_irradiance",
            "cloud_cover", "cloud_cover_low", "temperature_2m",
            "hour_sin", "hour_cos", "doy_sin", "doy_cos"),
    WND = c("wind_speed_80m", "wind_speed_120m", "wind_cubed_80m",
            "wdir80_sin", "wdir80_cos", "wind_gusts_10m",
            "temperature_80m", "surface_pressure",
            "hour_sin", "hour_cos", "doy_sin", "doy_cos"),
    WAT = c("precipitation", "temperature_2m", "snow_depth",
            "et0_fao_evapotranspiration",
            "hour_sin", "hour_cos", "doy_sin", "doy_cos"),
    GEO = c("temperature_2m", "hour_sin", "hour_cos", "doy_sin", "doy_cos")
  )
  
  
  today      <- Sys.Date()
  start_date <- today - past_days
  end_date   <- today + forecast_days
  
  HOURLY_VARS <- c(
    "shortwave_radiation", "direct_radiation", "diffuse_radiation",
    "direct_normal_irradiance", "global_tilted_irradiance",
    "cloud_cover", "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high",
    "sunshine_duration", "is_day", "uv_index",
    "wind_speed_10m", "wind_speed_80m", "wind_speed_120m", "wind_speed_180m",
    "wind_direction_10m", "wind_direction_80m",
    "wind_direction_120m", "wind_direction_180m",
    "wind_gusts_10m",
    "temperature_80m", "temperature_120m", "temperature_180m",
    "temperature_2m", "relative_humidity_2m", "dew_point_2m",
    "apparent_temperature", "wet_bulb_temperature_2m",
    "vapour_pressure_deficit", "surface_pressure", "pressure_msl",
    "precipitation", "rain", "showers", "snowfall", "snow_depth",
    "et0_fao_evapotranspiration",
    "weather_code", "precipitation_probability"
  )
  
  recent_weather <- weather_forecast(
    location = c(32.7157, -117.1611),   # San Diego
    start    = as.character(start_date),
    end      = as.character(end_date),
    hourly   = HOURLY_VARS,
    timezone = "America/Los_Angeles"
  )
  
  recent_weather_clean <- recent_weather %>%
    rename_with(~ str_remove(.x, "^hourly_"), starts_with("hourly_")) %>%
    rename(period_utc = datetime) %>%
    mutate(period_utc = with_tz(period_utc, "UTC")) %>%
    mutate(
      hour_pt    = hour(with_tz(period_utc, "America/Los_Angeles")),
      doy        = yday(with_tz(period_utc, "America/Los_Angeles")),
      hour_sin   = sin(2 * pi * hour_pt / 24),
      hour_cos   = cos(2 * pi * hour_pt / 24),
      doy_sin    = sin(2 * pi * doy / 365),
      doy_cos    = cos(2 * pi * doy / 365),
      wdir80_sin = sin(2 * pi * wind_direction_80m / 360),
      wdir80_cos = cos(2 * pi * wind_direction_80m / 360),
      wind_cubed_80m = wind_speed_80m^3
    )
  
 
  gen_recent <- data_generation_by_energy_source %>%
    filter(respondent == target_respondent,
           fueltype %in% RENEWABLE_FUELS) %>%
    rename(generation = value) %>%
    mutate(period_utc = ymd_hm(paste0(period, ":00"), tz = "UTC"))
  
  
  models <- list()
  for (fuel in names(FEATURES)) {
    mpath <- file.path(model_dir, paste0("xgb_", fuel, ".model"))
    if (file.exists(mpath)) {
      models[[fuel]] <- xgb.load(mpath)
    }
  }
  if (length(models) == 0) stop("No models found in ", model_dir)
  
  
  preds_list <- purrr::map_dfr(names(models), function(fuel) {
    feat <- intersect(FEATURES[[fuel]], names(recent_weather_clean))
    if (length(feat) == 0) return(NULL)
    
    X <- as.matrix(recent_weather_clean[, feat, drop = FALSE])
    tibble(
      period_utc = recent_weather_clean$period_utc,
      fueltype   = fuel,
      prediction = pmax(predict(models[[fuel]], X), 0)
    )
  })
  
  
  combined <- preds_list %>%
    left_join(
      gen_recent %>% select(period_utc, fueltype, generation),
      by = c("period_utc", "fueltype")
    ) %>%
    arrange(fueltype, period_utc)
  
  
  plot_df <- combined %>%
    pivot_longer(
      cols = c(generation, prediction),
      names_to = "series", values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      fuel_label = recode(fueltype, !!!FUEL_LABELS),
      period_pt  = with_tz(period_utc, "America/Los_Angeles"),
      series     = recode(series,
                          generation = "Actual",
                          prediction = "Predicted")
    )
  
  now_pt <- with_tz(Sys.time(), "America/Los_Angeles")
  p <- ggplot(plot_df, aes(x = period_pt, y = value,
                           color = series, linetype = series)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_vline(xintercept = as.numeric(now_pt),
               linetype = "dotted", color = "grey40") +
    facet_wrap(~ fuel_label, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("Actual"    = "#1f77b4",
                                  "Predicted" = "#d62728"), name = NULL) +
    scale_linetype_manual(values = c("Actual"    = "solid",
                                     "Predicted" = "dashed"), name = NULL) +
    scale_x_datetime(
      date_breaks = "12 hours",
      labels = function(x) {
        paste0(
          format(x, "%H", tz = "America/Los_Angeles")
        )
      }
    ) +
    labs(
      title    = paste0(target_respondent, " - San Diego"),
      subtitle = "Solido = actual (EIA) | Punteado = Modelo XGBoost | Vertical = hoy",
      x = NULL, y = "Generation (MWh)"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(4, "cm"),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y       = element_text(size = 7),
      strip.text        = element_text(size = 9, face = "bold"),
      panel.spacing     = unit(0.6, "lines"),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = 12),
      plot.subtitle     = element_text(size = 9),
      plot.margin       = margin(8, 8, 8, 8)
    )
  
  return(p)
}