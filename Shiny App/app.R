# Carregar bibliotecas necessárias
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(TTR)


load("combined_air_quality.Rdata")
load("daily_weather.Rdata")

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  
  titlePanel("Air Quality and Weather Data in Barcelona"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Select Station:", choices = unique(combined_air_quality$ESTACIO)),
      dateRangeInput("date_range", "Select Date Range:", start = min(combined_air_quality$Date), end = max(combined_air_quality$Date)),
      selectInput("pollutant", "Select Pollutant:", choices = unique(combined_air_quality$CODI_CONTAMINANT)),
      selectInput("weather_var", "Select Weather Variable:", choices = unique(daily_weather$VariableCode))
    ),
    
    mainPanel(
      fluidRow(
        column(12, card(card_header("Air Quality Data"), plotlyOutput("airQualityPlot", height = "400px")))
      ),
      fluidRow(
        column(12, card(card_header("Weather Data"), plotlyOutput("weatherPlot", height = "400px")))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive filtered data
  filtered_air_quality <- reactive({
    combined_air_quality %>%
      filter(ESTACIO == input$station, Date >= input$date_range[1] & Date <= input$date_range[2], CODI_CONTAMINANT == input$pollutant)
  })
  
  filtered_weather <- reactive({
    daily_weather %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2], VariableCode == input$weather_var)
  })
  
  # Plot outputs
  output$airQualityPlot <- renderPlotly({
    data <- filtered_air_quality()
    
    if (nrow(data) == 0) {
      # Criar um gráfico vazio com a mensagem de aviso
      p <- ggplot() +
        annotate("text", x = 1, y = 1, label = "The selected pollutant isn't measured at this station", size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = Date, y = Value)) +
        geom_line(color = "grey") +
        labs(title = paste("Air Quality for", input$pollutant), x = "Date", y = "Value") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$weatherPlot <- renderPlotly({
    data <- filtered_weather()
    data <- data %>% arrange(Date) %>%
      mutate(MovingAvg = SMA(DailyValue, n = 70))
    p <- ggplot(data, aes(x = Date, y = DailyValue)) +
      geom_line(color = "#C0C0C0") +
      geom_line(aes(y = MovingAvg), color = "red") +  # Adicionar a média móvel
      labs(title = paste("Weather Data for", input$weather_var), x = "Date", y = "Daily Value") +
      theme_minimal()
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
