library(shiny)
library(usmap)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinythemes)


make_us_map <- function(data){

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

# Create BA to states mapping
ba_states_df <- stack(ba_to_states) %>%
  rename(state = values, ba = ind) %>%
  mutate(ba = as.character(ba))


data <- data %>%
  mutate(
    datetime = ymd_h(period),
    value = as.numeric(value)
  )

# Get unique datetimes for the slider
unique_datetimes <- sort(unique(data$datetime))
datetime_labels <- format(unique_datetimes, "%Y-%m-%d %H:%M")

# Prepare all data by datetime and BA
grouped_all <- data %>%
  group_by(datetime, parent) %>%
  summarise(
    sum = sum(value, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Pre-calculate state values for all datetimes
state_values_all <- expand.grid(
  datetime = unique_datetimes,
  state = unique(ba_states_df$state),
  stringsAsFactors = FALSE
)

# Fill in BA values
for(i in 1:nrow(state_values_all)) {
  dt <- state_values_all$datetime[i]
  st <- state_values_all$state[i]
  
  # Find which BA this state belongs to
  ba <- ba_states_df$ba[ba_states_df$state == st][1]
  
  # Get the sum for this BA and datetime
  val <- grouped_all$sum[grouped_all$datetime == dt & grouped_all$parent == ba]
  
  if(length(val) > 0) {
    state_values_all$value[i] <- val[1]
  } else {
    state_values_all$value[i] <- NA
  }
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("US Electricity Demand by Balancing Authority"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Select Time"),
      
      sliderInput("time_slider",
                  "Hour:",
                  min = 1,
                  max = length(unique_datetimes),
                  value = length(unique_datetimes),
                  step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE),
                  ticks = FALSE),
      
      div(style = "margin-top: 10px;"),
      h5(textOutput("datetime_display")),
      
      hr(),
      
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats"),
      
      hr(),

    ),
    
    mainPanel(
      width = 9,
      plotOutput("us_map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value for current datetime
  current_datetime <- reactive({
    idx <- input$time_slider
    unique_datetimes[idx]
  })
  
  # Create map
  output$us_map <- renderPlot({
    dt <- current_datetime()
    
    # Get data for this datetime
    state_data <- state_values_all %>%
      filter(datetime == dt) %>%
      select(state, value)
    
    # Create the map
    p <- plot_usmap(data = state_data, values = "value", color = "black") +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        name = "Demand (MW)",
        na.value = "lightgray",
        labels = scales::comma
      ) +
      labs(
        title = paste("Electricity Demand by State"),
        subtitle = format(dt, "%B %d, %Y at %H:%M")
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5)
      )
    
    print(p)
  })
  
  # Display current datetime
  output$datetime_display <- renderText({
    dt <- current_datetime()
    paste("📅", format(dt, "%Y-%m-%d"), "\n🕐", format(dt, "%H:%M"))
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    dt <- current_datetime()
    
    state_data <- state_values_all %>%
      filter(datetime == dt)
    
    if(nrow(state_data) > 0) {
      total_demand <- sum(state_data$value, na.rm = TRUE)
      avg_demand <- mean(state_data$value, na.rm = TRUE)
      max_demand <- max(state_data$value, na.rm = TRUE)
      max_state <- state_data$state[which.max(state_data$value)]
      min_demand <- min(state_data$value, na.rm = TRUE)
      min_state <- state_data$state[which.min(state_data$value)]
      
      paste0(
        "Total Demand: ", format(round(total_demand), big.mark = ","), " MW\n",
        "Average: ", format(round(avg_demand), big.mark = ","), " MW\n",
        "Highest: ", max_state, " (", format(round(max_demand), big.mark = ","), " MW)\n",
        "Lowest: ", min_state, " (", format(round(min_demand), big.mark = ","), " MW)"
      )
    } else {
      "No data available"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
}