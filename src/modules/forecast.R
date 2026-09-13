library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(xgboost)

getwd()

# ---- Cargar modelo una sola vez -----------------------------------------
.ruta_modelos <- "modules/machine_learning/models"
.xgb_model    <- xgb.load(file.path(.ruta_modelos, "xgb_demand_us48.model"))
.xgb_features <- readRDS(file.path(.ruta_modelos, "features.rds"))

# ---- Predictor recursivo ------------------------------------------------
.predecir_1h <- function(datos_hist, tz = "UTC") {
  d <- datos_hist %>% arrange(period)
  prox <- tail(d$period, 1) + hours(1)
  
  nuevo <- data.frame(
    hour       = hour(prox),
    dow        = wday(prox),
    month      = month(prox),
    is_weekend = wday(prox) %in% c(1, 7),
    lag_1h     = tail(d$demand, 1),
    lag_24h    = tail(d$demand, 24)[1],
    lag_48h    = tail(d$demand, 48)[1],
    lag_168h   = tail(d$demand, 168)[1],
    roll_24h   = mean(tail(d$demand, 24)),
    roll_168h  = mean(tail(d$demand, 168))
  )
  nuevo <- nuevo[, .xgb_features, drop = FALSE]
  pred  <- predict(.xgb_model, as.matrix(nuevo))
  
  data.frame(period = prox, demand = as.numeric(pred))
}

.generar_forecast <- function(datos_hist, horizonte = 48) {
  d <- datos_hist %>% select(period, demand) %>% arrange(period)
  out <- vector("list", horizonte)
  for (h in seq_len(horizonte)) {
    paso <- .predecir_1h(d)
    out[[h]] <- paso
    d <- bind_rows(d, paso)
  }
  bind_rows(out)
}




make_forecast <- function(forecast_df,
                          horizonte    = 48,
                          horas_hist   = 96,
                          tz_salida    = "America/Mexico_City",
                          marcar_picos = TRUE) {
  
  # ---- 1) Parseo + limpieza ----------------------------------------------
  df <- forecast_df %>%
    filter(respondent == "US48", type == "D") %>%
    mutate(
      period = ymd_h(sub("T", " ", period)) %>%
        force_tz(tzone = "UTC"),
      demand = as.numeric(value)
    ) %>%
    arrange(period) %>%
    select(period, demand)
  
  # ---- 2) Forecast con el modelo XGBoost --------------------------------
  fc_df <- .generar_forecast(df, horizonte = horizonte)
  
  # ---- 3) Recorte del histórico + conversión a tz local -----------------
  hist_df <- df %>%
    mutate(period = with_tz(period, tz_salida)) %>%
    filter(period >= max(period) - hours(horas_hist))
  
  fc_df <- fc_df %>%
    mutate(period = with_tz(period, tz_salida))
  
  # Punto de unión para que las líneas se toquen
  punto_union <- hist_df %>% slice_tail(n = 1)
  fc_plot     <- bind_rows(punto_union, fc_df) %>% arrange(period)
  
  ultimo_hist <- max(hist_df$period)
  inicio      <- min(hist_df$period)
  fin         <- max(fc_plot$period)
  
  # ---- 4) Picos diarios sobre el histórico ------------------------------
  picos <- hist_df %>%
    arrange(period) %>%
    mutate(
      prev    = lag(demand),
      next_v  = lead(demand),
      es_pico = !is.na(prev) & !is.na(next_v) & demand > prev & demand > next_v
    ) %>%
    filter(es_pico) %>%
    mutate(dia = as.Date(period, tz = tz_salida)) %>%
    group_by(dia) %>%
    slice_max(demand, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(etiqueta = label_number(scale_cut = cut_short_scale())(demand))
  
  # ---- 5) Gráfica -------------------------------------------------------
  p <- ggplot() +
    geom_vline(
      xintercept = ultimo_hist,
      linetype   = "dashed", color = "grey40",
      linewidth  = 0.6
    ) +
    geom_line(
      data = hist_df, aes(period, demand),
      color = "steelblue", linewidth = 0.8
    ) +
    geom_line(
      data = fc_plot, aes(period, demand),
      color = "orange", linewidth = 0.9, linetype = "dashed"
    )
  
  if (marcar_picos && nrow(picos) > 0) {
    p <- p +
      geom_point(
        data = picos, aes(period, demand),
        color = "steelblue", size = 2.2, shape = 21,
        fill = "white", stroke = 1
      ) +
      geom_text(
        data = picos, aes(period, demand, label = etiqueta),
        vjust = -1.3, size = 3, color = "grey25", fontface = "bold"
      )
  }
  
  p +
    scale_x_datetime(
      date_labels = "%d\n%Hh",
      date_breaks = "6 hours",
      timezone    = tz_salida,
      limits      = c(inicio, fin)
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.05, 0.12))
    ) +
    labs(
      title    = "Demanda eléctrica US48",
      subtitle = sprintf(
        "Histórico + Forecast (%dh) — %s a %s (hora CDMX)",
        horizonte,
        format(inicio, "%d-%b %Hh"),
        format(fin,    "%d-%b %Hh")
      ),
      x = NULL, y = "Demanda (MWh)",
      caption = "🔵 Histórico      🟠 Forecast XGBoost      ┆ último dato      ● Pico diario"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}