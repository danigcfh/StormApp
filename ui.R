#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
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


# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Storm Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for selecting predictors
      selectInput("State", "Select state:", choices = unique(data$STATE)),
      selectInput("Month", "Select Month:", choices = unique(data$Month)),
      
      # Button to trigger prediction
      actionButton("predict_button", "Predict"),
      
      # Placeholder for output
      textOutput("prediction_text")
    ),
    
    mainPanel(
      # Placeholder for plots or additional information
    )
  )
)

