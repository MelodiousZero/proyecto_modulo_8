library(plotly)
library(dplyr)
library(lubridate)

make_us_map <- function(data) {
  
  ba_to_states <- list(
    BHBA = c("SD", "WY", "MT", "NE", "CO"),
    CISO = c("CA", "NV"),
    ERCO = c("TX"),
    MISO = c("IL", "IN", "MI", "MN", "WI", "IA", "MO", "ND", "SD", "AR",
             "KY", "MS", "MT", "OH", "PA", "WV", "LA", "TX", "AL", "GA",
             "TN", "FL", "NC", "SC"),
    NYIS = c("NY"),
    PJM  = c("PA", "NJ", "MD", "DE", "VA", "WV", "OH", "IN", "IL", "KY",
             "NC", "TN", "MI", "DC"),
    PNM  = c("NM"),
    SWPP = c("OK", "KS", "MO", "NE", "ND", "SD", "MT", "WY", "TX", "AR",
             "LA", "MS", "AL", "GA", "FL", "NC", "SC"),
    SWPW = c("KS", "CO", "NE", "NM", "TX", "OK")
  )
  
  ba_states_df <- stack(ba_to_states) %>%
    rename(state = values, ba = ind) %>%
    mutate(ba = as.character(ba), state = as.character(state))
  
  # --- Preprocesamiento ---
  data <- data %>%
    mutate(
      datetime = ymd_h(sub("T", " ", period)) %>%
        force_tz(tzone = "UTC") %>%
        with_tz(tzone  = "America/Mexico_City"),
      value = as.numeric(value)
    )
  
  unique_datetimes <- sort(unique(data$datetime))
  
  grouped_all <- data %>%
    group_by(datetime, parent) %>%
    summarise(sum = sum(value, na.rm = TRUE), .groups = "drop")
  
  # --- Grid estado x timestamp ---
  state_values_all <- expand.grid(
    datetime = unique_datetimes,
    state    = unique(ba_states_df$state),
    stringsAsFactors = FALSE
  ) %>%
    # CAMBIO 2: preservar la tz de salida (no forzar UTC)
    mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01",
                                 tz = "America/Mexico_City")) %>%
    left_join(ba_states_df, by = "state") %>%
    left_join(grouped_all, by = c("datetime", "ba" = "parent")) %>%
    rename(value = sum) %>%
    select(datetime, state, value) %>%
    arrange(datetime, state)
  
  # Frame legible para el slider (ahora en hora CDMX)
  state_values_all$frame_label <- format(state_values_all$datetime,
                                         "%Y-%m-%d %H:%M")
  
  zmin <- min(state_values_all$value, na.rm = TRUE)
  zmax <- max(state_values_all$value, na.rm = TRUE)
  
  colorscale <- list(
    c(0,   "blue"),
    c(0.5, "white"),
    c(1,   "red")
  )
  
  p <- plot_ly(
    data         = state_values_all,
    type         = "choropleth",
    locationmode = "USA-states",
    locations    = ~state,
    ids          = ~state,          # 👈 identificador estable entre frames
    z            = ~value,
    frame        = ~frame_label,
    colorscale   = colorscale,
    zmin         = zmin,
    zmax         = zmax,
    colorbar     = list(title = "Demanda (MW)", tickformat = ","),
    hovertemplate = paste0(
      "<b>%{location}</b><br>",
      "Demanda: %{z:,.0f} MW<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(text = "Demanda de electricidad por estado", x = 0.5),
      geo = list(
        scope      = "usa",
        projection = list(type = "albers usa"),
        showlakes  = TRUE,
        lakecolor  = "white"
      ),
      margin = list(l = 0, r = 0, t = 60, b = 0)
    ) %>%
    animation_opts(
      frame      = 1000,
      transition = 0,       # 👈 sin transición: evita el "no se colorea"
      redraw     = TRUE     # 👈 CLAVE para choropleths
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "Fecha: "),
      font = list(size = 12)
    ) %>%
    animation_button(
      x = 0, xanchor = "left",
      y = 0, yanchor = "bottom"
    )
  
  p
}