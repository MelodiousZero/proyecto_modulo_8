
library(dplyr)
library(plotly)

make_usmap_sales <- function(sales_to_customers_monthly) {

  
  latest_period <- max(sales_to_customers_monthly$period, na.rm = TRUE)
  
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  yr <- substr(latest_period, 1, 4)
  mo <- as.integer(substr(latest_period, 6, 7))
  latest_period_str <- paste(meses_es[mo], yr)   
  
  
  df_map <- sales_to_customers_monthly %>%
    filter(sectorid == "ALL",
           period == latest_period,
           stateid != "US") %>%
    group_by(stateid) %>%
    summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
  
  fig <- plot_ly(
    data = df_map,
    type = "choropleth",
    locations = ~stateid,
    locationmode = "USA-states",
    z = ~revenue,
    colorscale = "Viridis",
    colorbar = list(title = "Revenue"),
    hovertemplate = paste0(
      "<b>%{location}</b><br>",
      "Revenue: %{z:$,.0f}<extra></extra>"
    )
  ) %>%
    layout(
      title = paste0("Ingresos por estado en millones de dólares\n", latest_period_str),
      geo = list(scope = "usa", projection = list(type = "albers usa")),
      margin = list(l = 0, r = 0, t = 40, b = 0)
    )
  
  return(fig)
}