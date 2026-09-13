

library(flexdashboard)
library(dplyr)

US_TOTAL_COUNTIES <- 3144

make_value_box_incidents <- function(df) {
  

  affected <- df %>%
    mutate(metersaffected = suppressWarnings(as.numeric(metersaffected))) %>%
    filter(!is.na(metersaffected), metersaffected > 0) %>%
    distinct(state, county)
  
  n_affected <- nrow(affected)
  
  pct <- round(100 * n_affected / US_TOTAL_COUNTIES, 2)
  
  color <- dplyr::case_when(
    pct < 3  ~ "success",   
    pct < 10  ~ "warning",   
    TRUE     ~ "danger"     
  )
  
  icon_fa <- dplyr::case_when(
    pct < 3  ~ "fa-check-circle",   
    pct < 10  ~ "fa-exclamation-triangle",   
    TRUE     ~ "fa-fire"     
  )
  
  valueBox(
    value = paste0(pct, "%"),
    icon  = icon_fa,
    color = color,
    caption = paste0(
      n_affected, " de ", US_TOTAL_COUNTIES,
      " condados reportando problemas"
    )
  )
}

make_state_with_most_problems <- function(df,
                                          metric    = c("meters", "counties"),
                                          green_max = 1,
                                          yellow_max = 3) {
  
  metric <- match.arg(metric)
  
  df <- df %>%
    mutate(metersaffected = suppressWarnings(as.numeric(metersaffected))) %>%
    filter(!is.na(state), state != "")
  
  by_state <- df %>%
    filter(!is.na(metersaffected), metersaffected > 0) %>%
    group_by(state) %>%
    summarise(
      n_counties   = n_distinct(county),
      total_meters = sum(metersaffected, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    arrange(desc(if (metric == "meters") total_meters else n_counties))
  
  if (nrow(by_state) == 0) {
    return(valueBox(
      value   = "—",
      icon    = icon("fa-check-circle"),
      color   = "success",
      caption = "No active outages reported"
    ))
  }
  
  top          <- by_state[1, ]
  top_state    <- top$state
  top_meters   <- top$total_meters
  top_counties <- top$n_counties
  
 
  color <- dplyr::case_when(
    top_meters <  50000 ~ "success",
    top_meters < 200000 ~ "warning",
    TRUE                ~ "danger"
  )
  
  if (metric == "meters") {
    icon_nm <- "fa-bolt"
    caption <- paste0("Estado con más problemas: ",
      formatC(top_meters, format = "d", big.mark = ","),
      " medidores afectados en ",
      top_counties, " condados"
    )
  } else {
    icon_nm <- "fa-map-pin"
    caption <- paste0(
      top_counties, " condados affectados — ",
      formatC(top_meters, format = "d", big.mark = ","),
      " medidores afectados"
    )
  }
  
  valueBox(
    value   = top_state,
    icon    = "fa-map-pin",
    color   = color,
    caption = caption
  )
}