library(tidyverse)
library(lubridate)
library(xgboost)

df <- read_csv(
  "src/modules/machine_learning/ml_data/merged_generation_weather.csv",
  show_col_types = FALSE
) %>%
  rename(generation = value)

df <- df %>%
  filter(!is.na(generation), generation > 0) %>%
  mutate(
    period_utc = ymd_hms(period_utc, tz = "UTC"),
    hour_pt    = hour(with_tz(period_utc, "America/Los_Angeles")),
    dow        = wday(period_utc, week_start = 1),
    month      = month(period_utc),
    doy        = yday(period_utc),
    hour_sin   = sin(2 * pi * hour_pt / 24),
    hour_cos   = cos(2 * pi * hour_pt / 24),
    doy_sin    = sin(2 * pi * doy / 365),
    doy_cos    = cos(2 * pi * doy / 365),
    wdir80_sin = sin(2 * pi * wind_direction_80m / 360),
    wdir80_cos = cos(2 * pi * wind_direction_80m / 360),
    wind_cubed_80m = wind_speed_80m^3
  ) %>%
  arrange(period_utc)

features <- list(
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
  GEO = c("temperature_2m", "hour_sin", "hour_cos",
          "doy_sin", "doy_cos")
)

train_one <- function(fuel) {
  d <- df %>% filter(fueltype == fuel)
  if (nrow(d) < 500) {
    cat(sprintf("[%s] skipped — only %d rows\n", fuel, nrow(d)))
    return(NULL)
  }
  
  feat <- features[[fuel]]
  feat <- intersect(feat, names(d))   
  
  n <- nrow(d)
  cut <- floor(0.8 * n)
  train <- d[1:cut, ]
  test  <- d[(cut + 1):n, ]
  
  X_train <- as.matrix(train[, feat])
  y_train <- train$generation
  X_test  <- as.matrix(test[, feat])
  y_test  <- test$generation
  
  model <- xgboost(
    x = X_train, y = y_train,
    nrounds = 600,
    max_depth = 6,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = "reg:squarederror",
    verbose = 0,
    nthread = parallel::detectCores() - 1
  )
  
  pred <- predict(model, X_test)
  rmse <- sqrt(mean((pred - y_test)^2))
  mae  <- mean(abs(pred - y_test))
  r2   <- 1 - sum((pred - y_test)^2) / sum((y_test - mean(y_test))^2)
  mape <- mean(abs((pred - y_test) / pmax(y_test, 1))) * 100
  
  cat(sprintf("%s | n_train=%d n_test=%d | RMSE=%.1f  MAE=%.1f  R²=%.3f  MAPE=%.1f%%\n",
              fuel, nrow(train), nrow(test), rmse, mae, r2, mape))
  
  list(fuel = fuel, model = model, features = feat,
       test = test %>% mutate(pred = pred),
       metrics = c(rmse = rmse, mae = mae, r2 = r2, mape = mape))
}

results <- lapply(c("SUN", "SNB", "WND", "WAT", "GEO"), train_one)
names(results) <- c("SUN", "SNB", "WND", "WAT", "GEO")
results <- results[!sapply(results, is.null)]

imp <- xgb.importance(
  feature_names = results$SUN$features,
  model = results$SUN$model
)
print(imp)

preds <- bind_rows(lapply(results, function(r) r$test))
write_csv(preds, "src/modules/machine_learning/ml_data/predictions.csv")

dir.create("src/modules/machine_learning/models", showWarnings = FALSE, recursive = TRUE)
for (fuel in names(results)) {
  xgb.save(results[[fuel]]$model,
           paste0("src/modules/machine_learning/models/xgb_", fuel, ".model"))
}