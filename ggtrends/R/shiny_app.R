library(shiny)
library(dplyr)
library(ggtrends)

folder_path <- "C:/Users/Wiktoria/Desktop/ProjektR/ggtrends/data"
all_data <- read_all_files(folder_path)
processed_data <- process_df(all_data$data_list)
combined_df <- combine_data_frames(processed_data)

ui <- fluidPage(
  titlePanel("Analiza google trends"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("types", "Wybierz jakie dane Cię interesują:",
                         choices = unique(combined_df$type),
                         selected = character(0)),
      actionButton("select_all", "Zaznacz wszystkie"),
      actionButton("deselect_all", "Odznacz wszystkie"),
      br(),
      sliderInput("years_range", "Zakres lat:",
                  min = 2004,
                  max = 2024,
                  value = c(2004, 2024),
                  step = 1,
                  round = TRUE,
                  ticks = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres punktowy z linią trendu", plotOutput("trend_plot")),
        tabPanel("Parametry Statystyczne",
                 selectInput("statistic", "Wybierz statystykę:", choices = c("mean_value", "median_value", "sd_value")),
                 plotOutput("stat_plot")),
        tabPanel("Wykres świecznikowy", plotOutput("candlestick_plot")),
        tabPanel("Wykres czasowy", plotOutput("time_series_plot")),
        tabPanel("Wykres rozrzutu", plotOutput("scatter_plot")),
        tabPanel("Wykres Sezonowy", plotOutput("season_plot")),
        tabPanel("Wykres średnich", plotOutput("mean_plot")),
        tabPanel("Wykres rozkładu wartości", plotOutput("distribution_plot")),
        tabPanel("Tau Kendalla ",
                 plotOutput("tau_plot"),
                 textOutput("tau_result"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Zaznacz wszystkie typy
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "types", selected = unique(combined_df$type))
  })

  # Odznacz wszystkie typy
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "types", selected = character(0))
  })

  # Filtracja danych
  filtered_data <- reactive({
    req(input$types)
    combined_df %>%
      filter(type %in% input$types,
             year >= input$years_range[1],
             year <= input$years_range[2])
  })

  # Wykres punktowy z linią trendu
  output$trend_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_scatter_with_trend_plot(filtered_data())
  })

  # Wykres statystyk
  output$line_chart <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_line_chart_plot(filtered_data())
  })

  output$stat_plot <- renderPlot({
    create_stat_plot(calculate_statistics(filtered_data(), range_of_years), input$statistic, input$statistic)
  })

  # Wykres sezonowy
  output$season_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_season_plot(calculate_statistics(filtered_data(),season))
  })

  # Tau
  output$tau_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_tao(filtered_data())
  })

  output$tau_result <- renderText({
    req(nrow(filtered_data()) > 0)
    tau <- count_tau_kendall(filtered_data())
    paste("Współczynnik Tau Kendalla:", round(tau, 2))
  })

  # Wykres świecznikowy
  output$candlestick_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_candlestick_plot(filtered_data())
  })

  # Wykres czasowy
  output$time_series_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_time_series_plot(filtered_data())
  })

  # Wykres rozrzutu
  output$scatter_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_catter_plot(filtered_data())
  })

  # Wykres średnich
  output$mean_plot<- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_mean_plot(filtered_data(),calculate_mean_results(filtered_data()))
  })

  # Wykres rozkładu wartości
  output$distribution_plot<- renderPlot({
    req(nrow(filtered_data()) > 0)
    draw_value_distribution_plot(filtered_data())
  })
}

shinyApp(ui = ui, server = server)


