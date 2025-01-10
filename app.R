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
library(shinyWidgets)
library(fmsb)
library(tibble)

# Load data, not every row because it is too big
#taxi_data <- read_csv("data.csv", n_max = 100000)
#taxi_data <- read_csv("C:/Users/unaiz/Downloads/archive/yellow_tripdata_2015-01.csv", n_max = 200000)
taxi_data <- read_csv("data/yellow_tripdata_2015-01_short.csv")
nyc_taxi_neigh <- read_csv("data/yellow_tripdata_2015-01_short_neigh.csv")


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
          h2("Welcome to the NYC Taxi Data Visualization App!"), # Ensure the text here is a single string
          p("New York City's taxi system is a dynamic and essential aspect of urban mobility. With more than 400,000 trips daily in 2015, these iconic yellow cabs serve as one of the most frequently used modes of transportation. The vastness and complexity of the city's traffic require a well-organized and efficient approach, making the analysis of taxi data a powerful tool for optimizing operations."),
          p("Our app gives you the ability to explore and visualize various aspects of NYC's taxi data, offering insights into trip statistics, distribution patterns, and correlations. Whether you're a researcher, data enthusiast, or someone interested in urban transportation, this app provides an interactive and intuitive interface for uncovering key trends and making data-driven decisions."),
          p("Here are some of the key features you can explore:"),
          tags$ul(
            tags$li("Database Preview: Preview the dataset with variable descriptions for easy understanding."), # Ensure text inside li is a single string
            tags$li("Passenger Proportion by Day of the Week: View the distribution of passengers across the week in a bar chart."),
            tags$li("Average Trip Distance by Passenger Count: Analyze how passenger count affects the average trip distance, based on the day of the week."),
            tags$li("Taxi Pickup Heatmap: Visualize the density of taxi pickups across NYC."),
            tags$li("Comparing Trip Counts Across Two Days of the Week: See a comparison of the number of trips for any two days."),
            tags$li("Correlation Analysis: Explore relationships between key variables, including scatter plots and correlation matrices."),
            tags$li("Radar Chart of Mean Tips by Neighborhood: Get insights into tipping patterns across NYC's neighborhoods.")
          ),
          p("The app allows you to interact with the data to draw your own conclusions, making it an excellent tool for anyone interested in exploring the intersection of urban transportation and data science."),
          img(
            src = "yellow-cab.png", 
            alt = "NYC Taxi", 
            style = "width: 100%; max-width: 600px; margin-top: 20px; border-radius: 8px; border: 2px solid #f4f4f4; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"
          ),
          p("Explore the data and see how the city's taxi system works in real-time!")
        )
      )
    )
    ,
    
    
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
          "Variable Descriptions",
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
    tabPanel(div(icon("chart-simple"), "Passenger Count by Day"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "How common is it for people to share taxis? Does this depend on the day of the week?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "By analyzing the data, it seems like on weekdays its more rare for people to share taxis than on weekends. This is likely due to gropus of friends going to and returning from parties.")
             ),
             p(),
             plotOutput("proportion_plot", height = "600px")
    ),
    
    # Detailed insights tab
    tabPanel(div(icon("chart-simple"), "Trip Distance by Passenger Count"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "Do fewer passengers in a taxi tend to do longer trips? Is this influenced by the day of the week?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "By analyzing the data, no mayor differences can be found.")
             ),
             p(),
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
    
    
    
    
    tabPanel(div(icon("map"), "Pickups Heatmap"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "Where do most pick ups take place? Are there different patterns at different times or on different days?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "From the heatmap we can observe that most pickups take place in Manhattan. There are also some focuses at important places like both airports (JFK and LaGuardia). This seems to be the pattern across days and time.")
             ),
             p(),
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
    
    tabPanel(div(icon("chart-simple"), "Trip Counts Across Two Days of the Week"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "Are taxis specially demanded on some days of the week?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "By comparing different days of the week in the barplots one can observe that Fridays and Saturdays have considerably more trips than other days of the week.")
             ),
             p(),
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
    
    
    
    tabPanel(div(icon("magnifying-glass-chart"), "Correlation Analysis"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "Which variables are highly correlated? How do they compare?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "The most correlated variables are fare_amount, total_amount and trip_distance. This makes sense, because the longer the trip, the more expensive it is. Surprisingly, trip_duration and trip_distance do not seem too correlated to each other. This is likely due to heterocedastity caused by traffic jams, leading to high variances in trip duration.")
             ),
             p(),
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
    
    tabPanel(div(icon("chart-pie"), "Tip Amount by Pickup Neighborhood"),
             p(),
             # Question Box Section
             div(
               style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; width: 100%; max-width: 800px; margin: 0 auto;",
               h3(style = "text-align: center; font-weight: bold; color: #00796b;", "Where do clients leave the biggest tips?"),
               p(style = "text-align: center; color: #004d40; font-size: 16px;", 
                 "It seems like the biggest tips come from clients from Queens. This could be because it is far away, so the trips are more expensive in general. However, the most likely hypothesis is that Queen has very wealthy parts such as Forest Hills, Bayside, Douglaston, and select areas of Astoria. Therefore, clients have a lot of money and are more generous. This is backed by the fact that the most humble neighborhood, the Bronx, leaves the least tips.")
             ),
             p(),
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   inputId = "selected_neighborhoods",
                   label = "Select Neighborhoods to Plot:",
                   #choices = unique(nyc_taxi_neigh$neigh_origin),
                   #choices = c("Manhattan","Queens","Brooklyn","Bronx","Staten Island"),
                   choices = na.omit(unique(nyc_taxi_neigh$neigh_origin)),
                   multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE)
                 )
               ),
               mainPanel(
                 plotOutput("tip_plot")
               )
             )
    )
    
    
    
  )
)

server <- function(input, output, session) {
  
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
  
  
  
  
  
  # Dynamically update the choices for neighborhoods
  observe({
    updatePickerInput(session,
                      inputId = "selected_neighborhoods",
                      choices = na.omit(unique(nyc_taxi_neigh$neigh_origin))
    )
  })
  
  output$tip_plot <- renderPlot({
    # Filter data for selected neighborhoods
    filtered_data <- nyc_taxi_neigh %>%
      filter(neigh_origin %in% input$selected_neighborhoods)
    
    # Check if there's data to plot
    if (nrow(filtered_data) == 0) {
      plot.new()
      title("No data available for selected neighborhoods")
      return()
    }
    
    # Aggregate mean tip amount by neighborhood
    aggregated_data <- filtered_data %>%
      group_by(neigh_origin) %>%
      summarize(mean_tip = mean(tip_amount, na.rm = TRUE)) %>%
      arrange(desc(mean_tip))
    
    # Prepare data for radar chart
    radar_data <- aggregated_data %>%
      column_to_rownames(var = "neigh_origin") %>%
      as.data.frame()  # Ensure it's a data frame
    
    # Add max and min rows for radar chart
    max_tip <- max(radar_data$mean_tip, na.rm = TRUE) * 1.1
    min_tip <- 0
    radar_data <- rbind(rep(max_tip, ncol(radar_data)),
                        rep(min_tip, ncol(radar_data)),
                        t(radar_data))
    
    # Ensure radar_data is a proper data frame
    radar_data <- as.data.frame(radar_data)
    
    # Radar Chart
    radarchart(
      radar_data,
      axistype = 1,
      pcol = rgb(0.2, 0.5, 0.5, 0.9),
      pfcol = rgb(0.2, 0.5, 0.5, 0.5),
      plwd = 2,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, max_tip, length.out = 5),
      cglwd = 0.8,
      vlcex = 0.8
    )
    title("Radar Chart of Mean Tips by Neighborhood")
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
