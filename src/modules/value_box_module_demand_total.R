# modules/value_boxes.R

library(flexdashboard)
library(dplyr)
library(lubridate)
library(shiny)

#' Create a value box with dynamic color based on percentage change
create_value_box <- function(summary) {
  
  # Extract values from summary
  value <- summary$current
  pct_change <- summary$pct_change
  direction <- summary$direction
  
  # Determine color, icon, and text based on direction
  if (direction == "up") {
    color <- "danger"
    icon <- "fa-arrow-up"
    change_text <- paste0("+", pct_change, "% demand from prior hour")
  } else if (direction == "down") {
    color <- "success"
    icon <- "fa-arrow-down"
    change_text <- paste0(pct_change, "% demand from prior hour")
  } else {
    color <- "info"
    icon <- "fa-minus"
    change_text <- "No change from prior hour"
  }
  
  # Create the value box with dynamic values
  valueBox(
    value = paste0(format(round(value, 0), big.mark = ","), " MWh"),
    caption = change_text,
    icon = icon,
    color = color
  )
}

#' Get demand summary with percentage change
get_mwh_summary <- function(data) {
  
  data <- data %>%
    mutate(
      datetime = ymd_h(period),  # Parse "2026-09-10T07" correctly
      value = as.numeric(value)
    )
  
  grouped <- data %>%
    group_by(datetime) %>%
    summarise(
      sum = sum(value, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(desc(datetime))  # Newest to oldest
  
  change <- ((grouped$sum[1] - grouped$sum[2]) / grouped$sum[2]) * 100
  direction <- ifelse(change > 0, "up", ifelse(change < 0, "down", "neutral"))
  list(
    current = grouped$sum[1],
    pct_change = round(change, 2),
    direction = direction
  )
  
}

