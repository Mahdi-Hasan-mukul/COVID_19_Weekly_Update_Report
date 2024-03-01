library(shiny)
library(plotly)
library(gsheet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Fetch data from Google Sheets
new <- "https://docs.google.com/spreadsheets/d/1oN1RUGHosHNrIWvwYyO4fyDfAKBuYPyvmCWZPJJmgm8/edit#gid=1251585962"
df_gogle_sheet <- gsheet2tbl(new)

# Convert 'Date' column to Date format
df_gogle_sheet$Date <- as.Date(df_gogle_sheet$Date, "%d/%m/%Y")

# Define UI 
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  tabsetPanel(
    tabPanel("Monthly Averages",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year1",
                             "Select Year:",
                             min = 2020,
                             max = year(max(df_gogle_sheet$Date, na.rm = TRUE)),
                             value = 2020,
                             step = 1),
                 sliderInput("month1",
                             "Select Month:",
                             min = 1,
                             max = 12,
                             value = 3,
                             step = 1)
               ),
               mainPanel(
                 plotlyOutput("covid_line_plot"),
                 plotlyOutput("covid_bar_plot")
               )
             )
    ),
    tabPanel("Daily Trends",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year2",
                             "Select Year:",
                             min = 2020,
                             max = year(max(df_gogle_sheet$Date, na.rm = TRUE)),
                             value = 2020,
                             step = 1),
                 sliderInput("month2",
                             "Select Month:",
                             min = 1,
                             max = 12,
                             value = 3,
                             step = 1)
               ),
               mainPanel(
                 plotlyOutput("daily_new_cases_plot")
               )
             )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$covid_line_plot <- renderPlotly({
    # Filter the data based on selected year and month
    data_filtered <- df_gogle_sheet %>%
      filter(year(Date) == input$year1 & month(Date) == input$month1)
    
    # Aggregate data by day
    data_daily <- data_filtered %>%
      group_by(Date) %>%
      summarise(
        `Daily new cases` = mean(`Daily new cases`),
        `New Tests` = mean(`New Tests`),
        `Daily new deaths` = mean(`Daily new deaths`),
        `New Recoverd` = mean(`New Recoverd`)
      )
    
    # Line chart
    p <- plot_ly(data_daily, x = ~Date) %>%
      add_trace(y = ~`Daily new cases`, name = 'Daily new cases', type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
      add_trace(y = ~`New Tests`, name = 'New Tests', type = 'scatter', mode = 'lines', line = list(color = 'green')) %>%
      add_trace(y = ~`Daily new deaths`, name = 'Daily new deaths', type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
      add_trace(y = ~`New Recoverd`, name = 'New Recoverd', type = 'scatter', mode = 'lines', line = list(color = 'orange')) %>%
      layout(
        title = paste("Daily Averages of COVID-19 Metrics in", month.name[input$month1], input$year1),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Values")
      )
    
    p
  })
  
  output$covid_bar_plot <- renderPlotly({
    # Filter the data based on selected year
    data_year <- df_gogle_sheet %>%
      filter(year(Date) == input$year1)
    
    # Aggregate data by month
    data_monthly <- data_year %>%
      group_by(Month = month(Date, label = TRUE, abbr = FALSE)) %>%
      summarise(
        `Daily new cases` = mean(`Daily new cases`),
        `New Tests` = mean(`New Tests`),
        `Daily new deaths` = mean(`Daily new deaths`),
        `New Recoverd` = mean(`New Recoverd`)
      )
    
    # Bar chart
    p <- plot_ly(data_monthly, x = ~Month) %>%
      add_trace(y = ~`Daily new cases`, name = 'Daily new cases', type = 'bar') %>%
      add_trace(y = ~`New Tests`, name = 'New Tests', type = 'bar') %>%
      add_trace(y = ~`Daily new deaths`, name = 'Daily new deaths', type = 'bar') %>%
      add_trace(y = ~`New Recoverd`, name = 'New Recoverd', type = 'bar') %>%
      layout(
        title = paste("Monthly Average of COVID-19 Condition in", input$year1),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Values"),
        barmode = 'stack'
      )
    
    p
  })
  
  # New renderPlotly function for daily new cases, deaths, and recovered by daily new test
  output$daily_new_cases_plot <- renderPlotly({
    arrange_new <- df_gogle_sheet %>%
      select(Date, `New Tests`, `Daily new cases`, `Daily new deaths`, `New Recoverd`) %>%
      filter(year(Date) == input$year2 & month(Date) == input$month2) %>%
      gather(key = "variable", value = "value", -Date)
    
    b20 <- ggplot(arrange_new, aes(x = Date, y = value)) + 
      geom_line(aes(color = variable)) + geom_point(aes(color = variable)) + 
      scale_color_manual(values = c("#3333FF", "#330000","#990000","#808080")) +
      theme_minimal() +
      ggtitle("Daily New Cases, Death & Daily Recovered By Daily New Test")
    
    ggplotly(b20)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
