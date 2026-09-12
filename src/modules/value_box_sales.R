library(dplyr)

make_current_month_box <- function(sales_to_customers_monthly){
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid  = toupper(trimws(as.character(stateid))),
      sectorid = toupper(trimws(as.character(sectorid))),
      period   = trimws(as.character(period)),
      sales    = as.numeric(sales)
    ) %>%
    filter(sectorid == "ALL", stateid == "US")
  
  # --- Latest period + same month previous year ---
  latest_period <- max(df$period, na.rm = TRUE)   # e.g. "2026-09"
  yr <- as.integer(substr(latest_period, 1, 4))
  mo <- substr(latest_period, 6, 7)               # "09"
  prev_period <- paste0(yr - 1, "-", mo)          # "2025-09"
  
  
  # --- Labels ---
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  lbl_cur  <- paste(meses_es[as.integer(mo)], yr)
  lbl_prev <- paste(meses_es[as.integer(mo)], yr - 1)
  
  
  
  flexdashboard::valueBox(
    value = paste0(lbl_cur),
    icon     = "fa-calendar",
    color = "info"
  )
}


make_value_box_total_revenue_month <- function(sales_to_customers_monthly) {
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid  = toupper(trimws(as.character(stateid))),
      sectorid = toupper(trimws(as.character(sectorid))),
      period   = trimws(as.character(period)),
      sales    = as.numeric(sales)
    ) %>%
    filter(sectorid == "ALL", stateid == "US")
  
  # --- Latest period + same month previous year ---
  latest_period <- max(df$period, na.rm = TRUE)   # e.g. "2026-09"
  yr <- as.integer(substr(latest_period, 1, 4))
  mo <- substr(latest_period, 6, 7)               # "09"
  prev_period <- paste0(yr - 1, "-", mo)          # "2025-09"
  
  cur_sales  <- sum(df$sales[df$period == latest_period], na.rm = TRUE)
  prev_sales <- sum(df$sales[df$period == prev_period],  na.rm = TRUE)
  
  # --- % change (YoY) ---
  pct <- if (is.na(prev_sales) || prev_sales == 0) NA_real_ else (cur_sales - prev_sales) / prev_sales
  
  up <- !is.na(pct) && pct >= 0
  
  
  change_text <- if (is.na(pct)) {
    "Sin datos del mismo mes del año anterior."
  } else {
    sprintf(
      "%s%.1f%% de ingresos respecto al año anterior",
      if (up) "+" else "-",
      abs(pct) * 100
    )
  }
  
  # --- Labels ---
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  lbl_cur  <- paste(meses_es[as.integer(mo)], yr)
  lbl_prev <- paste(meses_es[as.integer(mo)], yr - 1)
  
  arrow   <- if (up) "\u25B2" else "\u25BC"                      # ▲ / ▼
  pct_str <- if (is.na(pct)) "N/A" else sprintf("%.1f%%",  abs(pct) * 100)
  
  # --- flexdashboard valueBox ---
  flexdashboard::valueBox(
    value = paste0(
      "$", formatC(cur_sales, format = "f", digits = 1, big.mark = ","),
      " M "
    ),
    caption = change_text,
    icon     = if (up) "fa-arrow-up" else "fa-arrow-down",
    color    = if (up) "success" else "danger"
  )
}


make_value_box_total_customers_month <- function(sales_to_customers_monthly) {
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid  = toupper(trimws(as.character(stateid))),
      sectorid = toupper(trimws(as.character(sectorid))),
      period   = trimws(as.character(period)),
      sales    = as.numeric(sales)
    ) %>%
    filter(sectorid == "ALL", stateid == "US")
  
  # --- Latest period + same month previous year ---
  latest_period <- max(df$period, na.rm = TRUE)   # e.g. "2026-09"
  yr <- as.integer(substr(latest_period, 1, 4))
  mo <- substr(latest_period, 6, 7)               # "09"
  prev_period <- paste0(yr - 1, "-", mo)          # "2025-09"
  
  cur_customers  <- sum(df$customers[df$period == latest_period], na.rm = TRUE)
  prev_customers <- sum(df$customers[df$period == prev_period],  na.rm = TRUE)
  
  # --- % change (YoY) ---
  pct <- if (is.na(prev_customers) || prev_customers == 0) NA_real_ else (cur_customers - prev_customers) / prev_customers
  
  up <- !is.na(pct) && pct >= 0
  
  
  change_text <- if (is.na(pct)) {
    "Sin datos del mismo mes del año anterior."
  } else {
    sprintf(
      "%s%.1f%% de personas respecto al año anterior",
      if (up) "+" else "-",
      abs(pct) * 100
    )
  }
  
  # --- Labels ---
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  lbl_cur  <- paste(meses_es[as.integer(mo)], yr)
  lbl_prev <- paste(meses_es[as.integer(mo)], yr - 1)
  
  arrow   <- if (up) "\u25B2" else "\u25BC"                      # ▲ / ▼
  pct_str <- if (is.na(pct)) "N/A" else sprintf("%.1f%%",  abs(pct) * 100)
  
  # --- flexdashboard valueBox ---
  flexdashboard::valueBox(
    value = paste0(
      formatC(cur_customers, format = "f", digits = 1, big.mark = ","),
      " personas "
    ),
    caption = change_text,
    icon     = if (up) "fa-arrow-up" else "fa-arrow-down",
    color    = if (up) "success" else "danger"
  )
}





make_value_box_total_sales_month <- function(sales_to_customers_monthly) {
  
  # --- Normalize ---
  df <- sales_to_customers_monthly %>%
    mutate(
      stateid  = toupper(trimws(as.character(stateid))),
      sectorid = toupper(trimws(as.character(sectorid))),
      period   = trimws(as.character(period)),
      sales    = as.numeric(sales)
    ) %>%
    filter(sectorid == "ALL", stateid == "US")
  
  # --- Latest period + same month previous year ---
  latest_period <- max(df$period, na.rm = TRUE)   # e.g. "2026-09"
  yr <- as.integer(substr(latest_period, 1, 4))
  mo <- substr(latest_period, 6, 7)               # "09"
  prev_period <- paste0(yr - 1, "-", mo)          # "2025-09"
  
  cur_sales  <- sum(df$sales[df$period == latest_period], na.rm = TRUE)
  prev_sales <- sum(df$sales[df$period == prev_period],  na.rm = TRUE)
  
  # --- % change (YoY) ---
  pct <- if (is.na(prev_sales) || prev_sales == 0) NA_real_ else (cur_sales - prev_sales) / prev_sales
  
  up <- !is.na(pct) && pct >= 0
  
  
  change_text <- if (is.na(pct)) {
    "Sin datos del mismo mes del año anterior."
  } else {
    sprintf(
      "%s%.1f%% de ventas respecto al año anterior",
      if (up) "+" else "-",
      abs(pct) * 100
    )
  }
  
  # --- Labels ---
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  lbl_cur  <- paste(meses_es[as.integer(mo)], yr)
  lbl_prev <- paste(meses_es[as.integer(mo)], yr - 1)
  
  arrow   <- if (up) "\u25B2" else "\u25BC"                      # ▲ / ▼
  pct_str <- if (is.na(pct)) "N/A" else sprintf("%.1f%%",  abs(pct) * 100)
  
  # --- flexdashboard valueBox ---
  flexdashboard::valueBox(
    value = paste0(
      formatC(cur_sales, format = "f", digits = 1, big.mark = ","),
      " mkh "
    ),
    caption = change_text,
    icon     = if (up) "fa-arrow-up" else "fa-arrow-down",
    color    = if (up) "success" else "danger"
  )
}




