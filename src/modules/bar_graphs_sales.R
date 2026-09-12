library(dplyr)
library(plotly)

make_sector_decomposition_bars <- function(sales_to_customers_monthly,
                                           year = NULL,
                                           metric = c("revenue", "sales")) {
  
  metric <- match.arg(metric)
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid    = toupper(trimws(as.character(stateid))),
      sectorid   = toupper(trimws(as.character(sectorid))),
      sectorName = trimws(as.character(sectorName)),
      period     = trimws(as.character(period)),
      value      = as.numeric(.data[[metric]])
    )
  
  # --- Default year = current year of latest period ---
  if (is.null(year)) {
    year <- as.integer(substr(max(df$period, na.rm = TRUE), 1, 4))
  }
  
  # --- Filter: US total, exclude ALL, only this year ---
  df_year <- df %>%
    filter(stateid == "US",
           sectorid != "ALL",
           substr(period, 1, 4) == as.character(year)) %>%
    group_by(period, sectorName) %>%
    summarise(value = sum(value, na.rm = TRUE) / 1e6,
              .groups = "drop") %>%
    arrange(period)
  
  if (nrow(df_year) == 0) {
    warning("Sin datos para el año ", year, " con sectorid != 'ALL' y stateid == 'US'.")
    return(NULL)
  }
  
  # --- Spanish month labels for x-axis ---
  meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
                "Jul","Ago","Sep","Oct","Nov","Dic")
  df_year <- df_year %>%
    mutate(
      month_lbl = meses_es[as.integer(substr(period, 6, 7))],
      month_lbl = factor(month_lbl, levels = meses_es)   # orden cronológico
    )
  
  # --- Side-by-side bars ---
  plot_ly(
    data = df_year,
    x = ~month_lbl,
    y = ~value,
    color = ~sectorName,
    type = "bar",
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "%{fullData.name}: $%{y:,.1f} M<extra></extra>"
    )
  ) %>%
    layout(
      barmode = "group",
      bargap = 0.15,        # espacio entre grupos de meses
      bargroupgap = 0.05,   # espacio entre barras del mismo mes
      xaxis = list(title = ""),
      yaxis = list(title = "Ingresos (millones USD)"),
      legend = list(title = list(text = "Sector")),
      margin = list(l = 60, r = 20, t = 50, b = 40)
    )
}




make_revenue_quarters_yoy <- function(sales_to_customers_monthly,
                                      metric = "revenue") {
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid  = toupper(trimws(as.character(stateid))),
      sectorid = toupper(trimws(as.character(sectorid))),
      period   = trimws(as.character(period)),
      value    = as.numeric(.data[[metric]])
    ) %>%
    filter(sectorid == "ALL", stateid == "US", !is.na(value)) %>%
    mutate(
      year    = as.integer(substr(period, 1, 4)),
      quarter = as.integer(substr(period, 7, 7)),
      q_lbl   = paste0("Q", quarter)
    ) %>%
    arrange(year, quarter)
  
  current_year <- max(df$year, na.rm = TRUE)
  last_q_cur   <- max(df$quarter[df$year == current_year], na.rm = TRUE)
  
  years <- sort(unique(df$year))
  
  # --- Colors: años previos en gris, actual en azul ---
  prev_years  <- setdiff(years, current_year)
  n_prev      <- length(prev_years)
  prev_colors <- if (n_prev > 0) {
    grDevices::colorRampPalette(c("#E0E0E0", "#9E9E9E"))(n_prev)
  } else character(0)
  
  color_map <- c(stats::setNames(prev_colors, as.character(prev_years)),
                 stats::setNames("#1F77B4", as.character(current_year)))
  width_map <- stats::setNames(rep(1.5, length(years)), as.character(years))
  width_map[as.character(current_year)] <- 3.5
  
  # --- Build plot ---
  fig <- plot_ly()
  
  for (yr in years) {
    d <- df %>% filter(year == yr) %>% arrange(quarter)
    is_current <- yr == current_year
    
    fig <- fig %>% add_trace(
      data = d,
      x = ~q_lbl,
      y = ~value,
      type = "scatter",
      mode = "lines+markers",
      name = as.character(yr),
      line = list(
        color = color_map[as.character(yr)],
        width = width_map[as.character(yr)],
        shape = "linear"
      ),
      marker = list(
        color = color_map[as.character(yr)],
        size  = if (is_current) 9 else 6
      ),
      hovertemplate = paste0(
        "<b>", yr, " %{x}</b><br>",
        "Ingresos: $%{y:,.0f} M<extra></extra>"
      )
    )
  }
  
  # --- Opcional: banda sombreada a partir de Q(last_q_cur + 1) ---
  # Marca visualmente que esos trimestres del año actual aún no existen.
  next_q <- last_q_cur + 1
  shapes <- if (next_q <= 4) {
    list(
      list(
        type = "rect",
        xref = "x", yref = "paper",
        x0 = next_q - 0.5, x1 = 4.5,
        y0 = 0, y1 = 1,
        fillcolor = "#F5F5F5",
        line = list(width = 0),
        layer = "below"
      )
    )
  } else list()
  
  fig %>% layout(
    xaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = paste0("Q", 1:4),
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Ingresos (millones USD)",
      gridcolor = "#EEEEEE",
      zeroline = FALSE
    ),
    legend = list(
      title = list(text = ""),
      orientation = "h",
      x = 0, y = -0.15
    ),
    shapes = shapes,
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    margin = list(l = 60, r = 20, t = 50, b = 60),
    hovermode = "x unified"
  )
}