library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(shiny)


capacity_vs_demand <- function(state_generation_capacity, state_demand_df) {
  
  # ---- Precompute once (outside the reactive) ----
  state_capacity <- state_generation_capacity %>%
    filter(
      energysourceid == "ALL",
      producertypeid == "TOT",
      period == max(period, na.rm = TRUE)      # keep only the latest year
    )
  
  state_demand <- make_state_demand_wide(state_demand_df)
  
  # ---- Build the state list from the data ----
  states_avail <- sort(unique(c(
    as.character(state_demand$state_id),
    as.character(state_capacity$stateId)
  )))
  states_avail <- states_avail[!is.na(states_avail) & nzchar(states_avail)]
  
  default_state <- if ("CA" %in% states_avail) "CA" else states_avail[1]
  
  # ---- UI ----
  ui <- fluidPage(
    titlePanel("Capacity vs Demand Speedometer"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state", "Select state:",
          choices  = states_avail,
          selected = default_state
        ),
        helpText("Demand is the last available hour. Capacity is the latest year.")
      ),
      mainPanel(
        plotOutput("speedo", height = "520px")
      )
    )
  )
  
  # ---- Server ----
  server <- function(input, output, session) {
    output$speedo <- renderPlot({
      req(input$state)
      speedometer(state_capacity, state_demand, state = input$state)
    })
  }
  
  # Returning the app object means simply calling this function
  # (without assigning it) will launch the app in an interactive session.
  shinyApp(ui, server)
}




library(ggplot2)
library(dplyr)

speedometer <- function(state_capacity, state_demand, state, max_pct = 150) {
  
  # --- Last hour of demand ---
  last_hour <- max(state_demand$hour, na.rm = TRUE)
  
  demand_last <- state_demand %>%
    filter(hour == last_hour) %>%
    mutate(state_id = as.character(state_id))
  
  capacity_all <- state_capacity %>%
    mutate(stateId = as.character(stateId))
  
  # --- Totals for the chosen state ---
  s_dem <- sum(demand_last$demand[demand_last$state_id == state], na.rm = TRUE)
  s_cap <- sum(capacity_all$capability[capacity_all$stateId == state], na.rm = TRUE)
  
  if (s_cap == 0 && s_dem == 0) {
    warning(sprintf("No data found for state '%s'. Check the state_id / stateId values.", state))
  }
  
  # --- Totals for US (all states combined) ---
  us_dem <- sum(demand_last$demand, na.rm = TRUE)
  us_cap <- sum(capacity_all$capability, na.rm = TRUE)
  
  # --- Build the 2-row data frame ---
  df <- data.frame(
    state_id = c(state, "US"),
    demand   = c(s_dem, us_dem),
    capacity = c(s_cap, us_cap),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      capacity = ifelse(capacity == 0, NA_real_, capacity),
      pct      = pmin((demand / capacity) * 100, max_pct)
    )
  
  # keep facet order: state first, then US
  df$state_id <- factor(df$state_id, levels = c(state, "US"))
  
  # --- Colored zones (arcs) ---
  zones <- data.frame(
    start = c(0,  60,  90),
    end   = c(60, 90, 150),
    color = c("#2ecc71", "#f1c40f", "#e74c3c")   # green / yellow / red
  )
  
  make_zone <- function(start, end, color, state, zone_id) {
    angles <- seq(pi * (1 - start / max_pct),
                  pi * (1 - end   / max_pct),
                  length.out = 50)
    r_in  <- 0.65
    r_out <- 1
    data.frame(
      x = c(r_in * cos(angles), rev(r_out * cos(angles))),
      y = c(r_in * sin(angles), rev(r_out * sin(angles))),
      state_id = state,
      fill = color,
      zone_id = paste0(state, "_", zone_id)
    )
  }
  
  states <- levels(df$state_id)
  
  zone_df <- do.call(rbind, lapply(states, function(s) {
    do.call(rbind, lapply(seq_len(nrow(zones)), function(i) {
      make_zone(zones$start[i], zones$end[i], zones$color[i], s, i)
    }))
  }))
  zone_df$state_id <- factor(zone_df$state_id, levels = states)
  
  # --- Needle ---
  needle_df <- df %>%
    mutate(
      angle = pi * (1 - pct / max_pct),
      xend  = 0.95 * cos(angle),
      yend  = 0.95 * sin(angle)
    )
  
  # --- Tick marks & labels ---
  tick_pct   <- seq(0, max_pct, by = 30)
  tick_angle <- pi * (1 - tick_pct / max_pct)
  ticks <- data.frame(
    pct  = tick_pct,
    x1   = 0.65 * cos(tick_angle),
    y1   = 0.65 * sin(tick_angle),
    x2   = 0.72 * cos(tick_angle),
    y2   = 0.72 * sin(tick_angle),
    xlab = 0.83 * cos(tick_angle),
    ylab = 0.83 * sin(tick_angle)
  )
  ticks_df <- do.call(rbind, lapply(states, function(s) cbind(ticks, state_id = s)))
  ticks_df$state_id <- factor(ticks_df$state_id, levels = states)
  
  # --- Plot ---
  ggplot() +
    geom_polygon(data = zone_df,
                 aes(x = x, y = y, fill = fill, group = zone_id),
                 alpha = 0.85) +
    scale_fill_identity() +
    geom_segment(data = ticks_df,
                 aes(x = x1, y = y1, xend = x2, yend = y2),
                 color = "grey20", linewidth = 0.4) +
    geom_text(data = ticks_df,
              aes(x = xlab, y = ylab, label = pct),
              size = 2.8, color = "grey20") +
    geom_segment(data = needle_df,
                 aes(x = 0, y = 0, xend = xend, yend = yend),
                 color = "black", linewidth = 1.2,
                 arrow = arrow(length = unit(0.15, "inches"),
                               type = "closed"),
                 lineend = "round") +
    geom_point(data = needle_df, aes(x = 0, y = 0),
               size = 3.5, color = "black") +
    geom_text(data = df,
              aes(x = 0, y = -0.35,
                  label = ifelse(is.na(pct), "N/A",
                                 sprintf("%.1f%%", pct))),
              size = 5, fontface = "bold") +
    geom_text(data = df,
              aes(x = 0, y = -0.62,
                  label = ifelse(is.na(capacity),
                                 sprintf("Demand: %.0f\nCapacity: N/A", demand),
                                 sprintf("Demand: %.0f\nCapacity: %.0f",
                                         demand, capacity))),
              size = 3) +
    facet_wrap(~ state_id, nrow = 1) +
    coord_fixed(xlim = c(-1.25, 1.25), ylim = c(-0.9, 1.15)) +
    labs(caption = paste("Last hour:", last_hour)) +
    theme_void(base_size = 12) +
    theme(
      strip.text   = element_text(face = "bold", size = 12),
      plot.caption = element_text(color = "grey40", hjust = 0.5),
      plot.margin  = margin(10, 10, 10, 10)
    )
}



make_state_demand_wide <- function(data) {
  
  ba_to_states <- list(
    BHBA = c("SD", "WY", "MT", "NE", "CO"),
    CISO = c("CA", "NV"),
    ERCO = c("TX"),
    MISO = c("IL", "IN", "MI", "MN", "WI", "IA", "MO", "ND", "SD", "AR", "KY", "MS", "MT", "OH", "PA", "WV", "LA", "TX", "AL", "GA", "TN", "FL", "NC", "SC"),
    NYIS = c("NY"),
    PJM = c("PA", "NJ", "MD", "DE", "VA", "WV", "OH", "IN", "IL", "KY", "NC", "TN", "MI", "DC"),
    PNM = c("NM"),
    SWPP = c("OK", "KS", "MO", "NE", "ND", "SD", "MT", "WY", "TX", "AR", "LA", "MS", "AL", "GA", "FL", "NC", "SC"),
    SWPW = c("KS", "CO", "NE", "NM", "TX", "OK")
  )
  
  ba_map <- tibble::enframe(ba_to_states, name = "parent", value = "state_id") %>%
    unnest(state_id)
  
  demand_by_state <- data %>%
    group_by(hour = period, parent) %>%
    summarise(demand = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(ba_map, by = "parent") %>%
    group_by(hour, parent) %>%
    mutate(demand = demand / n()) %>%   # remove this if you want full BA demand copied to each state
    ungroup() %>%
    group_by(hour, state_id) %>%
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") %>%
    select(hour, state_id, demand)
  
  demand_by_state
}