if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras") 
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("reshape2")) install.packages("reshape2")
if (!require("forcats")) install.packages("forcats")
if (!require("reactable")) install.packages("reactable")
if (!require("ggExtra")) install.packages("ggExtra")

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(forcats)
library(reactable) # For better table formatting
library(reactable) 
library(ggExtra)
library(vroom)

# Load data, not every row because it is too big
#taxi_data <- read_csv("data.csv", n_max = 100000)
taxi_data <- read_csv("C:/Users/unaiz/Downloads/archive/yellow_tripdata_2015-01.csv", n_max = 200000)



# Read a random sample (turns out it is not necessary, there is no pattern in the csv)
#set.seed(1234)  # Set seed for reproducibility
#taxi_data <- vroom("C:/Users/unaiz/Downloads/archive/yellow_tripdata_2015-01.csv", delim = ",") %>%
#  sample_n(300000)



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
           pickup_longitude <= nyc_boundaries$max_long & 
           trip_distance > 0 &
           total_amount > 0 &
           tip_amount >= 0
  ) %>%
  select(colnames(taxi_data))
  #select(pickup_latitude, pickup_longitude, dropoff_latitude, drpoff_longitude, tpep_pickup_datetime, tpep_dropoff_datetime, trip_distance, total_amount, tip_amount, fare_amount)



# Convert datetimes
taxi_pickup$tpep_pickup_datetime <- ymd_hms(taxi_pickup$tpep_pickup_datetime)
taxi_pickup$tpep_dropoff_datetime <- ymd_hms(taxi_pickup$tpep_dropoff_datetime)

# Calculate trip duration in minutes
taxi_pickup <- taxi_pickup %>%
  mutate(trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins"))) %>%
  filter(trip_duration > 0)  # Remove trips with invalid durations

# Preprocess the data
taxi_data$tpep_pickup_datetime <- as.POSIXct(taxi_data$tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
taxi_data$tpep_dropoff_datetime <- as.POSIXct(taxi_data$tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")

# Set locale to ensure day names are in English
Sys.setlocale("LC_TIME", "C")

# Extract day names in English
taxi_data$day_of_week <- weekdays(taxi_data$tpep_pickup_datetime)

# Add derived columns
taxi_data$trip_duration <- as.numeric(difftime(taxi_data$tpep_dropoff_datetime, taxi_data$tpep_pickup_datetime, units = "mins"))

# Remove rows with missing or invalid trip_duration and passenger_count values
data <- taxi_data %>%
  filter(!is.na(trip_duration) & trip_duration >= 0 & passenger_count > 0) %>% # Remove NA, negative trip_duration, and zero passengers
  select(passenger_count, trip_distance, fare_amount,total_amount,day_of_week,trip_duration)

# Balance the dataset: Ensure equal rows for all days
set.seed(123)
balanced_data <- data %>%
  group_by(day_of_week) %>%
  sample_n(size = min(1000, n()), replace = FALSE) %>%
  ungroup()

# Reorder days of the week from Monday to Sunday
ordered_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
balanced_data$day_of_week <- factor(balanced_data$day_of_week, levels = ordered_days)


# Shiny app
ui <- fluidPage(
  titlePanel("NYC Taxi Data Visualization"),
  tabsetPanel(
    # Homepage Tab
    tabPanel(
      div(icon("home"), "Home"), # Add a home icon and label the tab
      fluidRow(
        column(
          width = 12,
          h2("Welcome to the NYC Taxi Data Visualization App!"),
          p("Taxis play a vital role in urban transportation, offering a reliable and efficient means 
            of travel. In the United States, particularly in New York City, taxis are used significantly 
            more than in many European countries, reaching staggering numbers of more than 400.000 taxi 
            rides per day in 2015. Their ubiquity and accessibility make them an integral part of daily 
            commuting and tourism alike, making it really important to explore the best way to distribute 
            them around the city for example, to avoid excessive traffic and organise them in the best way 
            possible."),
          p("This app will let you freely explore the distribution and relation of different labels, giving 
            the ability to reach conclusions for yourself."),
          p("Key features include:"),
          tags$ul(
            tags$li("Heatmaps for visualizing taxi pickups and tips."),
            tags$li("Daily trip statistics for comparing trips on different days."),
            tags$li("Distribution of trip durations."),
            tags$li("Correlation analysis between trip metrics.")
          ),
          img(
            src = "yellow-cab.png", 
            alt = "NYC Taxi", 
            style = "width: 100%; max-width: 600px; margin-top: 20px;"
          )
        )
      )
    ),
    
    tabPanel(
      div(icon("table"), "Database Preview"),
      tabsetPanel(
        tabPanel(
          "Dataset Preview",
          fluidPage(
            fluidRow(
              column(4,
                     numericInput(
                       inputId = "num_rows",
                       label = "Number of rows to display:",
                       value = 10,
                       min = 1,
                       max = nrow(taxi_pickup)
                     )
              ),
              column(8,
                     dateRangeInput(
                       inputId = "date_range",
                       label = "Select Date Range:",
                       start = as.Date("2015-01-01"),
                       end = as.Date("2015-01-31"),
                       min = as.Date("2015-01-01"),
                       max = as.Date("2015-01-31"),
                       separator = " to ",
                       width = "100%"
                     )
              )
            ),
            hr(),
            reactableOutput("data_table")
          )
        ),
        tabPanel(
          "Field Descriptions",
          fluidPage(
            fluidRow(
              column(12,
                     reactableOutput("field_descriptions_table")
              )
            )
          )
        )
      )
    ),
    
    
    
    
    # Proportion visualization tab
    tabPanel("Passenger Proportion by Day",
             plotOutput("proportion_plot", height = "600px")
    ),
    
    # Detailed insights tab
    tabPanel("Detailed Insights by Day",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "selected_day",
                   "Select a Day:",
                   choices = ordered_days,
                   selected = "Monday"
                 )
               ),
               mainPanel(
                 h4("Aggregated Statistics"),
                 reactableOutput("day_stats_table"),  # Table output
                 plotOutput("day_trip_distribution")  # Plot output
               )
             )
    ),
    
    
    
    
    tabPanel("Pickups Heatmap",
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
    
    
    
    tabPanel("Correlation Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "Select X Variable:",
                             choices = c("trip_distance", "total_amount", "tip_amount", "trip_duration", "fare_amount")),
                 selectInput("y_var", "Select Y Variable:",
                             choices = c("trip_distance", "total_amount", "tip_amount", "trip_duration", "fare_amount")),
                 actionButton("update_corr", "Update Scatter Plot")
               ),
               mainPanel(
                 plotOutput("scatter_plot"),
                 plotOutput("corr_heatmap")
               )
             )
    ),
    
    tabPanel("Tip Heatmap",
             sidebarLayout(
               sidebarPanel(
                 selectInput("day_of_week_tip", "Select Day of the Week:",
                             choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             selected = "Monday"),
                 sliderInput("hour_of_day_tip", "Select Hour of the Day:",
                             min = 0, max = 23, value = c(0, 23), step = 1)
               ),
               mainPanel(
                 leafletOutput("tip_heatmap", height = "600px")
               )
             )
    )
    
    
    
  )
)

server <- function(input, output) {
  
  # Reactive data for filtering and limiting rows
  filtered_data <- reactive({
    data <- taxi_pickup
    
    # Filter by date range
    if (!is.null(input$date_range)) {
      # Add one day to the end date to include the entire last day
      adjusted_end_date <- as.POSIXct(input$date_range[2]) + days(1)
      data <- data %>%
        filter(tpep_pickup_datetime >= as.POSIXct(input$date_range[1]) &
                 tpep_pickup_datetime < adjusted_end_date)
    }
    
    # Limit the number of rows
    data <- head(data, n = input$num_rows)
    data
  })
  
  # Render the dataset table
  output$data_table <- renderReactable({
    reactable(
      filtered_data(),
      searchable = FALSE,  # No search bar since filtering is via the calendar
      pagination = FALSE,  # Controlled by `num_rows`
      highlight = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      defaultPageSize = input$num_rows
    )
  })
  
  # Static field descriptions table
  field_descriptions <- data.frame(
    Field_Name = c(
      "VendorID", "tpep_pickup_datetime", "tpep_dropoff_datetime",
      "Passenger_count", "Trip_distance", "Pickup_longitude",
      "Pickup_latitude", "RateCodeID", "Store_and_fwd_flag",
      "Dropoff_longitude", "Dropoff_latitude", "Payment_type",
      "Fare_amount", "Extra", "MTA_tax", "Improvement_surcharge",
      "Tip_amount", "Tolls_amount", "Total_amount"
    ),
    Description = c(
      "A code indicating the TPEP provider that provided the record. 1. Creative Mobile Technologies; 2. VeriFone Inc.",
      "The date and time when the meter was engaged.",
      "The date and time when the meter was disengaged.",
      "The number of passengers in the vehicle. This is a driver-entered value.",
      "The elapsed trip distance in miles reported by the taximeter.",
      "Longitude where the meter was engaged.",
      "Latitude where the meter was engaged.",
      "The final rate code in effect at the end of the trip. 1. Standard rate; 2. JFK; 3. Newark; 4. Nassau or Westchester; 5. Negotiated fare; 6. Group ride",
      "This flag indicates whether the trip record was held in vehicle memory before sending to the vendor, aka “store and forward,” because the vehicle did not have a connection to the server. Y= store and forward trip; N= not a store and forward trip",
      "Longitude where the meter was disengaged.",
      "Latitude where the meter was disengaged.",
      "A numeric code signifying how the passenger paid for the trip. 1.Credit card; 2. Cash; 3. No charge; 4. Dispute; 5. Unknown; 6. Voided trip",
      "The time-and-distance fare calculated by the meter.",
      "Miscellaneous extras and surcharges. Currently, this only includes the $0.50 and $1 rush hour and overnight charges.",
      "0.50 MTA tax that is automatically triggered based on the metered rate in use.",
      "0.30 improvement surcharge assessed trips at the flag drop.",
      "Tip amount – automatically populated for credit card tips. Cash tips are not included.",
      "Total amount of all tolls paid in the trip.",
      "The total amount charged to passengers. Does not include cash tips."
    )
  )
  
  # Render the field descriptions table
  output$field_descriptions_table <- renderReactable({
    reactable(
      field_descriptions,
      searchable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      defaultPageSize = 19
    )
  })
  
  
  
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
  
  # Scatter Plot with Outlier Removal
  observeEvent(input$update_corr, {
    output$scatter_plot <- renderPlot({
      # Dynamically filter the data to remove outliers based on percentiles
      filtered_data <- taxi_pickup %>%
        filter(
          between(!!sym(input$x_var), quantile(!!sym(input$x_var), 0.01, na.rm = TRUE), quantile(!!sym(input$x_var), 0.99, na.rm = TRUE)) &
            between(!!sym(input$y_var), quantile(!!sym(input$y_var), 0.01, na.rm = TRUE), quantile(!!sym(input$y_var), 0.99, na.rm = TRUE))
        )
      
      # Create the scatterplot
      ggplot(filtered_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(alpha = 0.5, color = "blue") +
        labs(
          title = paste("Scatter Plot of", input$x_var, "vs", input$y_var),
          x = input$x_var,
          y = input$y_var
        ) +
        theme_minimal()
    })
  })
  
  
  # Correlation Heatmap
  output$corr_heatmap <- renderPlot({
    # Select numerical columns for correlation
    numeric_vars <- taxi_pickup %>%
      select(trip_distance, total_amount, tip_amount, trip_duration, fare_amount) %>%
      na.omit()
    
    # Compute correlation matrix
    corr_matrix <- cor(numeric_vars)
    
    # Plot heatmap
    ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap",
           x = "",
           y = "",
           fill = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  output$tip_heatmap <- renderLeaflet({
    # Filter data based on selected day and hour
    filtered_data <- taxi_pickup %>%
      filter(wday(tpep_pickup_datetime) == match(input$day_of_week_tip, 
                                                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(hour(tpep_pickup_datetime) >= input$hour_of_day_tip[1] & hour(tpep_pickup_datetime) <= input$hour_of_day_tip[2]) %>%
      filter(tip_amount > 0)  # Only consider trips with tips
    
    # Base map
    m <- leaflet(data = filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # Add heatmap
    if (nrow(filtered_data) > 0) {
      m <- m %>%
        addHeatmap(
          lng = ~pickup_longitude,
          lat = ~pickup_latitude,
          intensity = ~tip_amount,  # Use tip_amount as the intensity
          blur = 20,
          max = 1,
          radius = 15
        )
    }
    
    m
  })
  
  # Proportion plot: Passenger count by day of week
  output$proportion_plot <- renderPlot({
    prop_data <- balanced_data %>%
      group_by(day_of_week, passenger_count) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(day_of_week) %>%
      mutate(percentage = count / sum(count) * 100)
    
    ggplot(prop_data, aes(x = day_of_week, y = percentage, fill = as.factor(passenger_count))) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), 
                position = position_stack(vjust = 0.5), size = 4) +
      scale_fill_brewer(palette = "Set3", name = "Passenger Count") +
      labs(
        title = "Proportion of Trips by Passenger Count for Each Day",
        x = "Day of the Week",
        y = "Percentage of Trips (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })
  
  # Table for aggregated statistics
  output$day_stats_table <- renderReactable({
    selected_data <- balanced_data %>%
      filter(day_of_week == input$selected_day)
    
    stats <- selected_data %>%
      group_by(passenger_count) %>%
      summarise(
        Trips = n(),
        `Avg. Trip Distance (miles)` = round(mean(trip_distance, na.rm = TRUE), 2),
        `Avg. Fare Amount (USD)` = round(mean(fare_amount, na.rm = TRUE), 2),
        `Avg. Total Amount (USD)` = round(mean(total_amount, na.rm = TRUE), 2),
        `Avg. Trip Duration (mins)` = round(mean(trip_duration, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    reactable(
      stats,
      columns = list(
        passenger_count = colDef(name = "Passenger Count"),
        Trips = colDef(name = "Number of Trips"),
        `Avg. Trip Distance (miles)` = colDef(),
        `Avg. Fare Amount (USD)` = colDef(),
        `Avg. Total Amount (USD)` = colDef(),
        `Avg. Trip Duration (mins)` = colDef()
      ),
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      defaultPageSize = 6
    )
  })
  
  # Distribution plot for trips
  output$day_trip_distribution <- renderPlot({
    selected_data <- balanced_data %>%
      filter(day_of_week == input$selected_day)
    
    # Compute average distance for each passenger count
    stats <- selected_data %>%
      group_by(passenger_count) %>%
      summarise(
        avg_distance = round(mean(trip_distance, na.rm = TRUE), 2),  # Calculate average distance
        .groups = "drop"
      ) %>%
      tidyr::complete(passenger_count = 1:7, fill = list(avg_distance = 0))  # Ensure all passenger counts are represented
    
    # Create the ggplot
    ggplot(stats, aes(x = as.factor(passenger_count), y = avg_distance)) +
      geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +  # Bar plot for avg. distance
      labs(
        title = paste("Average Trip Distance by Passenger Count on", input$selected_day),
        x = "Passenger Count",
        y = "Average Trip Distance (miles)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
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
