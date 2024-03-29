#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(randomForest)
library(lubridate)
library(readr)
library(stringr)

# Load the data
data <- read_csv("repdata_data_StormData.csv", col_types = cols(BGN_DATE = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

# Extract month from BGN_DATE and reorder the data frame
data <- data %>%
  mutate(Month = month(BGN_DATE)) %>%
  arrange(Month)

data <- data %>% mutate(Event = case_when(
  grepl("TORNADO", EVTYPE, ignore.case = TRUE) ~ "TORNADO",
  grepl("HURRICANE", EVTYPE, ignore.case = TRUE) ~ "HURRICANE",
  grepl("HEAT", EVTYPE, ignore.case = TRUE) ~ "HEAT",
  grepl("WIND", EVTYPE, ignore.case = TRUE) ~ "WIND",
  grepl("FLOOD", EVTYPE, ignore.case = TRUE) ~ "FLOOD",
  str_detect(EVTYPE, regex("STORM|LIGHTNING", ignore_case = TRUE)) ~ "STORM",
  str_detect(EVTYPE, regex("COLD|FREEZING|FREEZE|ICE|ICY|FROST|WINTER", ignore_case = TRUE)) ~ "COLD/WINTER",
  str_detect(EVTYPE, regex("HAIL|RAIN|SNOW|THUNDERSNOW|BLIZZARD", ignore_case = TRUE)) ~ "PRECIPITATIONS",
  str_detect(EVTYPE, regex("FIRE", ignore_case = TRUE)) ~ "WILD FIRES",
  TRUE ~ "OTHER"  # Default case if none of the conditions match
))

# Define server logic
function(input, output) {
  # Reactive value to store selected month
  selected_month <- reactive({
    # Convert input$Month to numeric
    as.numeric(input$Month)
  })
  
  # Reactive function to perform prediction
  predict_values <- eventReactive(input$predict_button, {
    # Get the selected month
    month <- selected_month()
    
    # Subset the data based on selected state and month
    subset_data <- data %>%
      filter(STATE == input$State, Month == month)
    
    # Check if subset_data is empty
    if (nrow(subset_data) == 0) {
      print("Error: Subset data is empty.")
      return(NULL)
    }
    
    # Select predictors
    predictors <- c("FATALITIES", "INJURIES", "Event")
    subset_data <- subset_data[, predictors]
    
    # Check if subset_data is empty after selecting predictors
    if (nrow(subset_data) == 0) {
      print("Error: Subset data is empty after selecting predictors.")
      return(NULL)
    }
    
    # Convert Event variable to factor explicitly
    subset_data$Event <- factor(subset_data$Event)
    
    # Fit a random forest model for classification
    model <- randomForest(Event ~ ., data = subset_data, ntree = 50)
    
    # Print trained model
    print(model)
    
    # Return the trained model
    return(model)
  })
  
  # Output predictions
  output$prediction_text <- renderText({
    # Perform prediction
    model <- predict_values()
    
    # Check if model is NULL
    if (is.null(model)) {
      return("Error: Unable to generate predictions.")
    }
    
    # Make predictions on new data (dummy data)
    new_data <- data.frame(FATALITIES = 0, INJURIES = 0, Event = "TORNADO")  # Dummy data
    predictions <- predict(model, newdata = new_data)
    
    # Find the most frequent event type in predictions
    most_likely_event <- names(sort(table(predictions), decreasing = TRUE))[1]
    
    # Filter subset_data for the most likely event type
    subset_data <- data %>%
      filter(STATE == input$State, Month == selected_month(), Event == most_likely_event)
    
    # Calculate predicted fatalities and injuries
    predicted_fatalities <- mean(subset_data$FATALITIES)
    predicted_injuries <- mean(subset_data$INJURIES)
    
    # Summarize the predicted values
    prediction_summary <- paste("Most likely event type:", most_likely_event)
    
    # Return the summary
    return(prediction_summary)
  })
}
