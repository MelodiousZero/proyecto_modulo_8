
library(ggplot2)
library(dplyr)
library(lubridate)

make_forecast <- function(forecast_df) {

  
  # 1) Filtrar US48 y tipos de interés, parsear fecha y valor
  df <- forecast_df %>%
    filter(respondent == "US48", type %in% c("D", "DF")) %>%
    mutate(
      period = ymd_h(sub("T", " ", period)),
      value  = as.numeric(value)
    ) %>%
    arrange(period)
  
  # 2) Separar histórico y pronóstico
  hist_df <- df %>% filter(type == "D")
  fc_df   <- df %>% filter(type == "DF")
  
  # 3) Quedarnos solo con los últimos días de histórico (por defecto 3)
  if (nrow(hist_df) > 0) {
    ultimo_hist <- max(hist_df$period, na.rm = TRUE)
    hist_reciente <- hist_df %>% filter(period >= ultimo_hist - days(3))
    
    # Añadir el último punto histórico al forecast para que las líneas conecten
    punto_union <- hist_reciente %>% slice_tail(n = 1) %>% mutate(type = "DF")
    fc_df <- bind_rows(punto_union, fc_df) %>% arrange(period)
  } else {
    hist_reciente <- hist_df
  }
  
  # 4) Graficar
  ggplot() +
    geom_line(
      data = hist_reciente,
      aes(x = period, y = value),
      color = "steelblue", linewidth = 0.8
    ) +
    geom_line(
      data = fc_df,
      aes(x = period, y = value),
      color = "orange", linewidth = 0.9, linetype = "dashed"
    ) +
    scale_x_datetime(
      date_labels = "%d-%b\n%Hh",
      date_breaks = "6 hours"
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = "Demanda eléctrica US48",
      subtitle = "Histórico reciente y pronóstico day-ahead",
      x        = NULL,
      y        = "Demanda (MWh)",
      caption = "🔵 Histórico (D)   |   🟠 Pronóstico (DF)"    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x     = element_text(angle = 0, hjust = 0.5, size = 9),
      panel.grid.minor = element_blank(),
      plot.title      = element_text(face = "bold")
    )
}