library(plotly)
library(dplyr)
library(lubridate)
library(htmlwidgets)   

make_us_map <- function(data,
                        frame_inicial = c("ahora", "inicio", "ultimo")) {
  
  frame_inicial <- match.arg(frame_inicial)
  
  subba_to_states <- list(
    # CISO
    "PGAE" = c("CA"),
    "SCE"  = c("CA"),
    "SDGE" = c("CA"),
    "VEA"  = c("NV", "CA"),
    
    # PNM
    "ACMA" = c("NM"),
    "CYGA" = c("NM"),
    "Frep" = c("NM"),
    "Jica" = c("NM"),
    "KAFB" = c("NM"),
    "KCEC" = c("NM"),
    "LAC"  = c("NM"),
    "PNM"  = c("NM"),
    "TSGT" = c("CO", "NE", "NM", "WY"),
    
    # BHBA
    "BASI" = c("ND", "SD", "MT", "WY", "NE", "CO", "IA", "MN"),
    "SDE"  = c("SD"),
    "WYE"  = c("WY"),
    
    # MISO
    "0001" = c("IN"),
    "0004" = c("WI"),
    "0006" = c("MO"),
    "0027" = c("MN", "ND", "SD", "MT", "AR"),
    "0035" = c("IA", "IL"),
    "8910" = c("LA", "TX", "MS"),
    
    # SWPP
    "CSWS" = c("OK", "AR", "LA", "TX"),
    "EDE"  = c("MO", "KS", "OK", "AR"),
    "GRDA" = c("OK"),
    "INDN" = c("MO"),
    "KACY" = c("KS"),
    "KCPL" = c("MO", "KS"),
    "LES"  = c("NE"),
    "MPS"  = c("MO"),
    "NPPD" = c("NE"),
    "OKGE" = c("OK", "AR"),
    "OPPD" = c("NE"),
    "SECI" = c("KS"),
    "SPRM" = c("MO"),
    "SPS"  = c("TX", "NM", "OK", "KS"),
    "WAUE" = c("ND", "SD", "MT"),
    "WFEC" = c("OK", "TX"),
    "WR"   = c("KS"),
    
    # SWPW
    "PRPA" = c("CO"),
    "WACM" = c("CO", "MT", "WY", "NE", "KS", "NM"),
    "WAUW" = c("MT", "ND", "SD", "WY"),
    
    # ERCO
    "COAS" = c("TX"),
    "EAST" = c("TX"),
    "FWES" = c("TX"),
    "NCEN" = c("TX"),
    "NRTH" = c("TX"),
    "SCEN" = c("TX"),
    "SOUT" = c("TX"),
    "WEST" = c("TX"),
    
    # NYIS
    "ZONA" = c("NY"),
    "ZONB" = c("NY"),
    "ZONC" = c("NY"),
    "ZOND" = c("NY"),
    "ZONE" = c("NY"),
    "ZONF" = c("NY"),
    "ZONG" = c("NY"),
    "ZONH" = c("NY"),
    "ZONI" = c("NY"),
    "ZONJ" = c("NY"),
    "ZONK" = c("NY"),
    
    # PJM
    "AE"   = c("NJ"),
    "AEP"  = c("OH", "IN", "MI", "WV", "VA", "KY", "TN"),
    "AP"   = c("PA", "WV", "MD", "VA"),
    "ATSI" = c("OH", "PA", "WV", "MD"),
    "BC"   = c("MD"),
    "CE"   = c("IL"),
    "DAY"  = c("OH"),
    "DEOK" = c("OH", "KY"),
    "DOM"  = c("VA", "NC"),
    "DPL"  = c("DE", "MD", "VA"),
    "DUQ"  = c("PA"),
    "EKPC" = c("KY"),
    "JC"   = c("NJ"),
    "ME"   = c("PA"),
    "PE"   = c("PA"),
    "PEP"  = c("DC", "MD"),
    "PL"   = c("PA"),
    "PN"   = c("PA"),
    "PS"   = c("NJ"),
    "RECO" = c("NJ")
  )
  
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
  
  ba_states_df <- stack(subba_to_states) %>%
    rename(state = values, ba = ind) %>%
    mutate(ba = as.character(ba), state = as.character(state))
  
  data <- data %>%
    mutate(
      datetime = ymd_h(sub("T", " ", period)) %>%
        force_tz(tzone = "UTC") %>%
        with_tz(tzone  = "America/Mexico_City"),
      value = as.numeric(value)
    )
  
  unique_datetimes <- sort(unique(data$datetime))
  
  grouped_all <- data %>%
    group_by(datetime, subba) %>%
    summarise(sum = sum(value, na.rm = TRUE), .groups = "drop")
  
  state_values_all <- expand.grid(
    datetime = unique_datetimes,
    state    = unique(ba_states_df$state),
    stringsAsFactors = FALSE
  ) %>%
    mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01",
                                 tz = "America/Mexico_City")) %>%
    left_join(ba_states_df, by = "state") %>%
    left_join(grouped_all, by = c("datetime", "ba" = "subba")) %>%
    rename(value = sum) %>%
    select(datetime, state, value) %>%
    arrange(datetime, state)
  
  state_values_all$frame_label <- format(state_values_all$datetime,
                                         "%Y-%m-%d %H:%M")
  
  zmin <- min(state_values_all$value, na.rm = TRUE)
  zmax <- max(state_values_all$value, na.rm = TRUE)
  
  colorscale <- list(c(0, "blue"), c(0.5, "white"), c(1, "red"))
  
  p <- plot_ly(
    data         = state_values_all,
    type         = "choropleth",
    locationmode = "USA-states",
    locations    = ~state,
    ids          = ~state,
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
      transition = 0,
      redraw     = TRUE
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "Fecha: "),
      font = list(size = 12)
    ) %>%
    animation_button(
      x = 0, xanchor = "left",
      y = 0, yanchor = "bottom"
    )
  
  frame_labels <- sort(unique(state_values_all$frame_label))
  
  target_index <- switch(
    frame_inicial,
    "inicio" = 0L,
    "ultimo" = length(frame_labels) - 1L,
    "ahora"  = {
      ahora <- with_tz(Sys.time(), "America/Mexico_City")
      ts    <- as.POSIXct(frame_labels, tz = "America/Mexico_City")
      which.min(abs(as.numeric(ts - ahora))) - 1L  
    }
  )
  
  onRender(
    p,
    sprintf(
      "function(el, x) {
         var idx = %d;
         function jump() {
           var sliders = el.layout && el.layout.sliders;
           if (!sliders || !sliders.length) return;
           var steps = sliders[0].steps || [];
           if (steps.length <= idx) return;
           var frameName = steps[idx].label;
           Plotly.animate(el, [frameName], {
             frame:      {duration: 0, redraw: true},
             transition: {duration: 0},
             mode:       'immediate'
           });
         }
         // Pequeño delay por si Plotly aún no terminó de inicializar
         setTimeout(jump, 50);
       }",
      target_index
    )
  )
}