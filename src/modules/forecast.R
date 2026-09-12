library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

make_forecast <- function(forecast_df,
                          tz_salida = "America/Mexico_City",
                          marcar_picos = TRUE) {
  
  # ---- 1) Parseo ----------------------------------------------------------
  df <- forecast_df %>%
    filter(respondent == "US48", type %in% c("D", "DF")) %>%
    mutate(
      period = ymd_h(sub("T", " ", period)) %>%
        force_tz(tzone = "UTC") %>%
        with_tz(tzone = tz_salida),
      value  = as.numeric(value)
    ) %>%
    arrange(period)
  
  hist_df <- df %>% filter(type == "D")
  fc_df   <- df %>% filter(type == "DF")
  
  inicio <- min(df$period, na.rm = TRUE)
  fin    <- max(df$period, na.rm = TRUE)
  
  ultimo_hist <- if (nrow(hist_df) > 0) max(hist_df$period, na.rm = TRUE) else NA
  
  hist_reciente <- hist_df %>% filter(period >= inicio)
  
  if (!is.na(ultimo_hist)) {
    punto_union <- hist_reciente %>% slice_tail(n = 1) %>% mutate(type = "DF")
    fc_df <- bind_rows(punto_union, fc_df) %>% arrange(period)
  }
  
  # ---- 2) Detección de picos (máximo local por día) ----------------------
  picos <- hist_reciente %>%
    arrange(period) %>%
    mutate(
      prev   = lag(value),
      next_v = lead(value),
      es_pico = !is.na(prev) & !is.na(next_v) & value > prev & value > next_v
    ) %>%
    filter(es_pico) %>%
    mutate(dia = as.Date(period, tz = tz_salida)) %>%
    group_by(dia) %>%
    slice_max(value, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(etiqueta = label_number(scale_cut = cut_short_scale())(value))
  
  # ---- 3) Gráfica ---------------------------------------------------------
  p <- ggplot() +
    geom_vline(
      xintercept = ultimo_hist,
      linetype   = "dashed", color = "grey40",
      linewidth  = 0.6, na.rm = TRUE
    ) +
    geom_line(
      data = hist_reciente,
      aes(period, value),
      color = "steelblue", linewidth = 0.8
    ) +
    geom_line(
      data = fc_df,
      aes(period, value),
      color = "orange", linewidth = 0.9, linetype = "dashed"
    )
  
  # Capa de picos (solo si hay y se pidió)
  if (marcar_picos && nrow(picos) > 0) {
    p <- p +
      geom_point(
        data = picos, aes(period, value),
        color = "steelblue", size = 2.2, shape = 21,
        fill = "white", stroke = 1
      ) +
      geom_text(
        data = picos, aes(period, value, label = etiqueta),
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
      expand = expansion(mult = c(0.05, 0.12))   # aire arriba para etiquetas
    ) +
    labs(
      title    = "Demanda eléctrica US48",
      subtitle = sprintf(
        "Histórico (D) + Pronóstico day-ahead (DF) — %s a %s (hora CDMX)",
        format(inicio, "%d-%b %Hh"),
        format(fin,    "%d-%b %Hh")
      ),
      x = NULL, y = "Demanda (MWh)",
      caption = "🔵 Histórico (D)      🟠 Pronóstico (DF)      ┆ último dato D      ● Pico diario"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}