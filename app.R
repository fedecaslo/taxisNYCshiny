if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("forcats")) install.packages("forcats")
if (!require("reactable")) install.packages("reactable")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)
library(reactable) 

# Load the dataset
data <- read_csv("nyc_taxi.csv")

# Preprocess the data
data$tpep_pickup_datetime <- as.POSIXct(data$tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
data$tpep_dropoff_datetime <- as.POSIXct(data$tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")

# Set locale to ensure day names are in English
Sys.setlocale("LC_TIME", "C")

# Extract day names in English
data$day_of_week <- weekdays(data$tpep_pickup_datetime)

# Add derived columns
data$trip_duration <- as.numeric(difftime(data$tpep_dropoff_datetime, data$tpep_pickup_datetime, units = "mins"))

# Remove rows with missing or invalid trip_duration and passenger_count values
data <- data %>%
  filter(!is.na(trip_duration) & trip_duration >= 0 & passenger_count > 0) # Remove NA, negative trip_duration, and zero passengers

# Balance the dataset: Ensure equal rows for all days
set.seed(123)
balanced_data <- data %>%
  group_by(day_of_week) %>%
  sample_n(size = min(1000, n()), replace = FALSE) %>%
  ungroup()

# Reorder days of the week from Monday to Sunday
ordered_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
balanced_data$day_of_week <- factor(balanced_data$day_of_week, levels = ordered_days)

# Define UI
ui <- fluidPage(
  titlePanel("NYC Taxi Data Analysis"),
  
  tabsetPanel(
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
                 reactableOutput("day_stats_table"),
                 plotOutput("day_trip_distribution")
               )
             )
    ),
    
    # Trip Duration vs Total Amount tab
    tabPanel("Trip Duration vs Total Amount",
             plotOutput("trip_duration_plot", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
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
  
  # Aggregated statistics for the selected day
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
      defaultPageSize = 6 # Display 6 rows per page
    )
  })
  
  
  # Distribution of trips by passenger count for the selected day
  output$day_trip_distribution <- renderPlot({
    selected_data <- balanced_data %>%
      filter(day_of_week == input$selected_day)
    
    ggplot(selected_data, aes(x = as.factor(passenger_count))) +
      geom_bar(fill = "skyblue", width = 0.7) +
      labs(
        title = paste("Trip Distribution by Passenger Count on", input$selected_day),
        x = "Passenger Count",
        y = "Number of Trips"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
  })
  
  # Trip Duration vs Total Amount plot
  output$trip_duration_plot <- renderPlot({
    ggplot(balanced_data, aes(x = trip_duration, y = total_amount)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", col = "blue", se = FALSE) +
      labs(
        title = "Trip Duration vs Total Amount",
        x = "Trip Duration (minutes)",
        y = "Total Amount (USD)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
