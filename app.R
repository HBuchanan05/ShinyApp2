#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

data_hosp_time <- read.csv("AccidentDataV1") |>
  filter(HOUR < 90, HOSP_HR < 80) |>
  mutate(time_diff = case_when(
    HOSP_HR < HOUR ~ HOSP_HR - HOUR + 24,
    TRUE ~ HOSP_HR - HOUR
  ), .keep = "all")


ui <- fluidPage(

    # Application title
    titlePanel("Motor Accident Deaths Involving EMT Transport"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("diff_time",
                        "EMT Time Difference - Accident to Hospital",
                        min = 0,
                        max = 23,
                        value = c(0,2))
        , 
        selectInput("measure", "Plot Measure", choices = c("Count", "Average Fatalities")),
        checkboxGroupInput("filter_month", "Month - Select", choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                           selected = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))), 
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("PLOT")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$PLOT <- renderPlot({
      used_data <- data_hosp_time |>
        filter(MONTHNAME %in% input$filter_month, time_diff >= input$diff_time[1], time_diff <= input$diff_time[2])

      if(input$measure == "Count")  {
        plot <- used_data |>
          ggplot(aes(x = time_diff)) +
          geom_bar()
      } else if(input$measure == "Average Fatalities") {
        plot <- used_data |>
          group_by(time_diff) |>
          summarize(avg_deaths = mean(FATALS)) |>
          ggplot(aes(x = time_diff, y = avg_deaths)) +
          geom_col()
      }
      
      plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
