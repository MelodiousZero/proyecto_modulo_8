
library(dplyr)
library(lubridate)
library(ggplot2)
library(xgboost)
library(vip)

set.seed(42)


datos <- read.csv("src/modules/machine_learning/ml_data/daily_hourly_historic.csv") %>%
  mutate(period = ymd_h(period, tz = "UTC")) %>%
  filter(!is.na(period)) %>%
  arrange(period) %>%
  select(period, demand = value)

cat("Filas:", nrow(datos), "\n")
cat("Rango:", format(min(datos$period)), "→", format(max(datos$period)), "\n")
cat("NAs en demand:", sum(is.na(datos$demand)), "\n")


datos %>%
  mutate(hour = hour(period)) %>%
  ggplot(aes(hour, demand, group = hour)) +
  geom_boxplot() +
  labs(title = "Demanda US48 por hora del día", y = "MW", x = "Hora")

datos %>%
  mutate(dow = wday(period, label = TRUE)) %>%
  ggplot(aes(dow, demand, group = dow)) +
  geom_boxplot() +
  labs(title = "Demanda US48 por día de semana", y = "MW", x = NULL)

datos %>%
  ggplot(aes(period, demand)) +
  geom_line(alpha = 0.3) +
  labs(title = "Serie completa US48", y = "MW", x = NULL)


df <- datos %>%
  mutate(
    hour       = hour(period),
    dow        = wday(period),            # 1 = domingo
    month      = month(period),
    is_weekend = dow %in% c(1, 7),
    
    lag_1h     = lag(demand, 1),
    lag_24h    = lag(demand, 24),
    lag_48h    = lag(demand, 48),
    lag_168h   = lag(demand, 168),
    
    roll_24h   = zoo::rollmeanr(demand, 24,  fill = NA),
    roll_168h  = zoo::rollmeanr(demand, 168, fill = NA),
    
    target     = lead(demand, 1)          # predecir próxima hora
  ) %>%
  na.omit()

cat("Filas tras features:", nrow(df), "\n")


cut_point <- floor(0.8 * nrow(df))
train <- df[1:cut_point, ]
test  <- df[(cut_point + 1):nrow(df), ]

cat("Train:", nrow(train), "| Test:", nrow(test), "\n")
cat("Train hasta:", format(max(train$period)), "\n")
cat("Test desde: ", format(min(test$period)), "\n")

mae_baseline <- mean(abs(test$demand - test$target))          # lag 0
mae_lag24    <- mean(abs(test$lag_24h - test$target))

cat("Baseline lag_0   MAE:", round(mae_baseline), "MW\n")
cat("Baseline lag_24h MAE:", round(mae_lag24), "MW\n")


features <- c("hour","dow","month","is_weekend",
              "lag_1h","lag_24h","lag_48h","lag_168h",
              "roll_24h","roll_168h")

X_train <- as.matrix(train[, features])
y_train <- train$target
X_test  <- as.matrix(test[, features])
y_test  <- test$target

xgb <- xgboost(
  data      = X_train,
  label     = y_train,
  nrounds   = 800,
  max_depth = 6,
  eta       = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose   = 0
)

pred_xgb <- predict(xgb, X_test)

mae_xgb  <- mean(abs(pred_xgb - y_test))
rmse_xgb <- sqrt(mean((pred_xgb - y_test)^2))

cat("\n--- XGBoost ---\n")
cat("MAE :", round(mae_xgb), "MW\n")
cat("RMSE:", round(rmse_xgb), "MW\n")
cat("Mejora vs baseline lag_0  :",
    round(100 * (1 - mae_xgb / mae_baseline), 1), "%\n")
cat("Mejora vs baseline lag_24h:",
    round(100 * (1 - mae_xgb / mae_lag24), 1), "%\n")


p <- test %>%
  mutate(pred = pred_xgb) %>%
  slice_head(n = 24 * 14) %>%
  ggplot(aes(period)) +
  geom_line(aes(y = target, color = "Real"), linewidth = 0.6) +
  geom_line(aes(y = pred,   color = "Predicho"), linewidth = 0.6) +
  labs(title = "XGBoost: forecast vs real (2 semanas de test)",
       y = "MW", x = NULL, color = NULL) +
  theme_minimal()

ggsave("src/figures/xgb_test_2weeks.pdf", p,
       width = 7, height = 3.2, units = "in", device = cairo_pdf)

test %>%
  mutate(pred  = pred_xgb,
         error = pred - target,
         hour  = hour(period)) %>%
  ggplot(aes(hour, error, group = hour)) +
  geom_boxplot() +
  labs(title = "Error por hora del día", y = "MW", x = "Hora") +
  theme_minimal()


imp <- xgb.importance(feature_names = features, model = xgb)
print(imp)

xgb.plot.importance(imp, top_n = 10,
                    main = "Importancia de features — XGBoost")








xgb.save(xgb, "src/modules/machine_learning/models/xgb_demand_us48.model")
saveRDS(features, "src/modules/machine_learning/models/features.rds")
saveRDS(
  list(
    mae_train = mae_xgb,
    trained_at = Sys.time(),
    train_end = max(train$period)
  ),
  "src/modules/machine_learning/models/metadata.rds"
)



