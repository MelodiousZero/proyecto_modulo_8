library(dplyr)
library(tidyr)
library(ggplot2)


capacity_vs_demand <- function(state_generation_capacity,
                               state_demand_df,
                               max_pct = 150) {
  
  # ---- Precompute once ----
  state_capacity <- state_generation_capacity %>%
    filter(
      energysourceid == "ALL",
      producertypeid == "TOT",
      period == max(period, na.rm = TRUE)
    )
  
  state_demand <- make_state_demand_wide(state_demand_df)
  
  # ---- Calcular pct para TODOS los estados (para encontrar el máximo) ----
  last_hour <- max(state_demand$hour, na.rm = TRUE)
  
  demand_last <- state_demand %>%
    filter(hour == last_hour) %>%
    mutate(state_id = as.character(state_id))
  
  capacity_all <- state_capacity %>%
    mutate(stateId = as.character(stateId))
  
  # Demanda y capacidad por estado
  demand_by_state <- demand_last %>%
    group_by(state_id) %>%
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop")
  
  capacity_by_state <- capacity_all %>%
    group_by(stateId) %>%
    summarise(capacity = sum(capability, na.rm = TRUE), .groups = "drop") %>%
    rename(state_id = stateId)
  
  pct_by_state <- full_join(demand_by_state, capacity_by_state,
                            by = "state_id") %>%
    mutate(
      demand   = replace_na(demand, 0),
      capacity = ifelse(is.na(capacity) | capacity == 0, NA_real_, capacity),
      pct      = (demand / capacity) * 100
    ) %>%
    filter(!is.na(pct), !is.na(state_id), nzchar(state_id))
  
  # El estado con el porcentaje más alto
  top_state <- pct_by_state %>%
    arrange(desc(pct)) %>%
    slice(1) %>%
    pull(state_id)
  
  if (length(top_state) == 0) top_state <- "US"  # fallback
  
  # ---- Facets: top_state primero, luego US ----
  speedometer_grid(state_capacity, state_demand,
                   states  = c(top_state, "US"),
                   max_pct = max_pct)
}


speedometer_grid <- function(state_capacity, state_demand,
                             states = c("US"), max_pct = 150) {
  
  last_hour <- max(state_demand$hour, na.rm = TRUE)
  
  demand_last <- state_demand %>%
    filter(hour == last_hour) %>%
    mutate(state_id = as.character(state_id))
  
  capacity_all <- state_capacity %>%
    mutate(stateId = as.character(stateId))
  capacity_all_no_us <- state_capacity %>%
    mutate(stateId = as.character(stateId)) %>%
    filter(stateId != "US")
  
  # ---- US totals ----
  us_dem <- sum(demand_last$demand, na.rm = TRUE)
  us_cap <- capacity_all %>%
    filter(stateId == "US") %>%
    summarise(capacity = sum(capability, na.rm = TRUE)) %>%
    pull(capacity)  
  # ---- Filas por estado ----
  per_state <- lapply(states, function(s) {
    if (s == "US") {
      data.frame(state_id = "US", demand = us_dem, capacity = us_cap)
    } else {
      d  <- sum(demand_last$demand[demand_last$state_id == s],  na.rm = TRUE)
      cp <- sum(capacity_all_no_us$capability[capacity_all_no_us$stateId == s], na.rm = TRUE)
      data.frame(state_id = s, demand = d, capacity = cp)
    }
  })
  
  df <- do.call(rbind, per_state) %>%
    mutate(
      capacity = ifelse(capacity == 0, NA_real_, capacity),
      pct      = pmin((demand / capacity) * 100, max_pct)
    )
  
  df$state_id <- factor(df$state_id, levels = states)
  
  # ---- Zonas ----
  zones <- data.frame(
    start = c(0,  60,  90),
    end   = c(60, 90, 150),
    color = c("#2ecc71", "#f1c40f", "#e74c3c")
  )
  
  make_zone <- function(start, end, color, state, zone_id) {
    angles <- seq(pi * (1 - start / max_pct),
                  pi * (1 - end   / max_pct),
                  length.out = 50)
    r_in  <- 0.65
    r_out <- 1
    data.frame(
      x        = c(r_in * cos(angles), rev(r_out * cos(angles))),
      y        = c(r_in * sin(angles), rev(r_out * sin(angles))),
      state_id = state,
      fill     = color,
      zone_id  = paste0(state, "_", zone_id)
    )
  }
  
  zone_df <- do.call(rbind, lapply(states, function(s) {
    do.call(rbind, lapply(seq_len(nrow(zones)), function(i) {
      make_zone(zones$start[i], zones$end[i], zones$color[i], s, i)
    }))
  }))
  zone_df$state_id <- factor(zone_df$state_id, levels = states)
  
  # ---- Agujas ----
  needle_df <- df %>%
    mutate(
      angle = pi * (1 - pct / max_pct),
      xend  = 0.95 * cos(angle),
      yend  = 0.95 * sin(angle)
    )
  
  # ---- Ticks ----
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
  
  # ---- Plot ----
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
                                 sprintf("Demanda: %.0f\nCapacidad: N/A", demand),
                                 sprintf("Demanda: %.0f\nCapacidad: %.0f",
                                         demand, capacity))),
              size = 3) +
    facet_wrap(~ state_id, nrow = 1) +
    coord_fixed(xlim = c(-1.25, 1.25), ylim = c(-0.9, 1.15)) +
    theme_void(base_size = 12) +
    theme(
      strip.text   = element_text(face = "bold", size = 12),
      plot.caption = element_text(color = "grey40", hjust = 0.5),
      plot.margin  = margin(10, 10, 10, 10)
    )
}





# -------------------------------------------------------------------
# make_state_demand_wide: SIN CAMBIOS
# -------------------------------------------------------------------
make_state_demand_wide <- function(data) {
  
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
  
  ba_map <- tibble::enframe(ba_to_states, name = "parent", value = "state_id") %>%
    unnest(state_id)
  
  demand_by_state <- data %>%
    group_by(hour = period, parent) %>%
    summarise(demand = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(ba_map, by = "parent") %>%
    group_by(hour, parent) %>%
    mutate(demand = demand / n()) %>%
    ungroup() %>%
    group_by(hour, state_id) %>%
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") %>%
    select(hour, state_id, demand)
  
  demand_by_state
}