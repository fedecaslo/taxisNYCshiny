if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras") 
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Load data, not every row because it is too big
taxi_data <- read_csv("C:/Users/unaiz/Downloads/archive/yellow_tripdata_2015-01.csv", n_max = 100000)

# Define NYC boundaries to drop locations outside the area
nyc_boundaries <- list(
  min_lat = 40.5774,  
  max_lat = 45.01585, 
  min_long = -74.25909, 
  max_long = -73.700272
)

# Filter rows inside NYC boundaries
taxi_pickup <- taxi_data %>%
  filter(!is.na(pickup_latitude) & !is.na(pickup_longitude) & 
           pickup_latitude >= nyc_boundaries$min_lat & 
           pickup_latitude <= nyc_boundaries$max_lat & 
           pickup_longitude >= nyc_boundaries$min_long & 
           pickup_longitude <= nyc_boundaries$max_long) %>%
  select(pickup_latitude, pickup_longitude, tpep_pickup_datetime, tpep_dropoff_datetime)

# Convert datetimes
taxi_pickup$tpep_pickup_datetime <- ymd_hms(taxi_pickup$tpep_pickup_datetime)
taxi_pickup$tpep_dropoff_datetime <- ymd_hms(taxi_pickup$tpep_dropoff_datetime)

# Calculate trip duration in minutes
taxi_pickup <- taxi_pickup %>%
  mutate(trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins"))) %>%
  filter(trip_duration > 0)  # Remove trips with invalid durations

# Shiny app
ui <- fluidPage(
  titlePanel("NYC Taxi Data Visualization"),
  tabsetPanel(
    tabPanel("Heatmap",
             sidebarLayout(
               sidebarPanel(
                 selectInput("day_of_week", "Select Day of the Week:",
                             choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             selected = "Monday"),
                 sliderInput("hour_of_day", "Select Hour of the Day:",
                             min = 0, max = 23, value = c(0, 23), step = 1)
               ),
               mainPanel(
                 leafletOutput("heatmap", height = "600px")
               )
             )
    ),
    
    tabPanel("Daily Trip Statistics",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range", "Select Date Range:",
                                start = min(taxi_pickup$tpep_pickup_datetime),
                                end = max(taxi_pickup$tpep_pickup_datetime)),
                 selectInput("week_day_1", "Select First Day of the Week:",
                             choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             selected = "Monday"),
                 selectInput("week_day_2", "Select Second Day of the Week:",
                             choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             selected = "Tuesday"),
                 actionButton("update_stats", "Update Plot")
               ),
               mainPanel(
                 plotOutput("trip_plot")
               )
             )
    ),
    
    tabPanel("Trip Duration",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("duration_bins", "Number of Bins:",
                             min = 10, max = 100, value = 30, step = 5),
                 numericInput("max_duration", "Maximum Duration to Display (minutes):",
                              value = 120, min = 1)
               ),
               mainPanel(
                 plotOutput("duration_histogram")
               )
             )
    )
  )
)

server <- function(input, output) {
  output$heatmap <- renderLeaflet({
    filtered_data <- taxi_pickup %>%
      filter(wday(tpep_pickup_datetime) == match(input$day_of_week, 
                                                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(hour(tpep_pickup_datetime) >= input$hour_of_day[1] & hour(tpep_pickup_datetime) <= input$hour_of_day[2])
    
    m <- leaflet(data = filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (nrow(filtered_data) > 0) {
      bounds <- getBounds(filtered_data$pickup_latitude, filtered_data$pickup_longitude)
      m <- m %>%
        setView(lng = bounds$center_long, lat = bounds$center_lat, zoom = 12) %>%
        fitBounds(lng1 = bounds$min_long, lat1 = bounds$min_lat, lng2 = bounds$max_long, lat2 = bounds$max_lat)
    } else {
      m <- m %>%
        setView(lng = -74.0060, lat = 40.7128, zoom = 12) # Default view
    }
    
    m %>%
      addHeatmap(
        lng = ~pickup_longitude,
        lat = ~pickup_latitude,
        blur = 20,
        max = 0.05,
        radius = 15
      )
  })
  
  observeEvent(input$update_stats, {
    filtered_data <- taxi_pickup %>%
      filter(tpep_pickup_datetime >= input$date_range[1],
             tpep_pickup_datetime <= input$date_range[2])
    
    filtered_data_1 <- filtered_data %>%
      filter(wday(tpep_pickup_datetime) == match(input$week_day_1, 
                                                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    filtered_data_2 <- filtered_data %>%
      filter(wday(tpep_pickup_datetime) == match(input$week_day_2, 
                                                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    trips_per_hour_1 <- filtered_data_1 %>%
      group_by(hour = hour(tpep_pickup_datetime)) %>%
      summarize(total_trips = n(), .groups = 'drop')
    
    trips_per_hour_2 <- filtered_data_2 %>%
      group_by(hour = hour(tpep_pickup_datetime)) %>%
      summarize(total_trips = n(), .groups = 'drop')
    
    combined_data <- full_join(trips_per_hour_1, trips_per_hour_2, by = "hour", 
                               suffix = c("_day1", "_day2")) %>%
      tidyr::pivot_longer(cols = starts_with("total_trips"), names_to = "day", values_to = "total_trips")
    
    output$trip_plot <- renderPlot({
      ggplot(combined_data, aes(x = hour, y = total_trips, fill = day)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        labs(title = paste("Comparison of Trips on", input$week_day_1, "and", input$week_day_2),
             x = "Hour of the Day",
             y = "Total Trips") +
        scale_fill_manual(values = c("blue", "orange"), labels = c(input$week_day_1, input$week_day_2)) +
        theme_minimal() +
        theme(legend.title = element_blank())
    })
  })
  
  output$duration_histogram <- renderPlot({
    filtered_data <- taxi_pickup %>%
      filter(trip_duration <= input$max_duration)
    
    ggplot(filtered_data, aes(x = trip_duration)) +
      geom_histogram(bins = input$duration_bins, fill = "blue", alpha = 0.7, color = "black") +
      labs(title = "Distribution of Trip Durations",
           x = "Trip Duration (minutes)",
           y = "Number of Trips") +
      theme_minimal()
  })
}
                       

# obtain limits to center the map
getBounds <- function(latitudes, longitudes) {
  return(list(
    min_lat = min(latitudes, na.rm = TRUE),
    max_lat = max(latitudes, na.rm = TRUE),
    min_long = min(longitudes, na.rm = TRUE),
    max_long = max(longitudes, na.rm = TRUE),
    center_lat = mean(latitudes, na.rm = TRUE),
    center_long = mean(longitudes, na.rm = TRUE)
  ))
}


shinyApp(ui = ui, server = server)
