library("ggtrends")

folder_path <- "C:/Users/Wiktoria/Desktop/ProjektR/ggtrends/data"
all_data <- read_all_files(folder_path)
combined_df <- process_df(all_data$data_list)

ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
      body {
        background-color: #E6E6FA; /* Lavender color for background */
      }
      .panel {
        background-color: #F5F5F5; /* Light grey for panel backgrounds */
        border-color: #D3D3D3; /* Light grey for panel borders */
      }
      .panel-heading {
        background-color: #D8BFD8; /* Thistle color for panel heading */
        border-color: #D3D3D3; /* Light grey for panel borders */
      }
      .checkbox {
        color: #4B0082; /* Indigo color for checkbox text */
      }
      .btn {
        background-color: #DDA0DD; /* Plum color for buttons */
        color: #4B0082; /* Indigo color for button text */
        border-color: #D3D3D3; /* Light grey for button borders */
      }
      .btn:hover {
        background-color: #BA55D3; /* Medium orchid for button hover */
      }
      .form-control, .well {
        background-color: #FFFFFF; /* White background for inputs and well panels */
      }
      .navbar-default {
        background-color: #E6E6FA; /* Lavender color for navbar */
        border-color: #D3D3D3; /* Light grey for navbar borders */
      }
      .navbar-default .navbar-brand, .navbar-default .navbar-text {
        color: #4B0082; /* Indigo color for navbar text */
      }
    "
    )
  )),
  
  titlePanel("Analiza Google Trends"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "types",
        "Wybierz jakie dane Cię interesują:",
        choices = unique(combined_df$type),
        selected = character(0)
      ),
      actionButton("select_all", "Zaznacz wszystkie"),
      actionButton("deselect_all", "Odznacz wszystkie"),
      br(),
      sliderInput(
        "years_range",
        "Zakres lat:",
        min = 2004,
        max = 2024,
        value = c(2004, 2024),
        step = 1,
        round = TRUE,
        ticks = TRUE
      ),
      actionButton("draw", "Rysuj") 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres punktowy z linią trendu", plotOutput("trend_plot")),
        tabPanel(
          "Parametry Statystyczne",
          selectInput(
            "statistic",
            "Wybierz statystykę:",
            choices = c("mean_value", "median_value", "sd_value")
          ),
          plotOutput("stat_plot")
        ),
        tabPanel("Wykres świecznikowy", plotOutput("candlestick_plot")),
        tabPanel("Wykres czasowy", plotOutput("time_series_plot")),
        tabPanel("Wykres rozrzutu", plotOutput("scatter_plot")),
        tabPanel("Wykres Sezonowy", plotOutput("season_plot")),
        tabPanel("Wykres średnich", plotOutput("mean_plot")),
        tabPanel("Wykres rozkładu wartości", plotOutput("distribution_plot")),
        tabPanel(
          "Tau Kendalla",
          plotOutput("tau_plot"),
          textOutput("tau_result")
        )
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
  
  # Filtracja danych na podstawie wybranych typów i zakresu lat
  filtered_data <- reactive({
    req(input$types)
    combined_df %>%
      filter(type %in% input$types,
             year >= input$years_range[1],
             year <= input$years_range[2])
  })
  
  # Obliczenia i rysowanie wykresów po naciśnięciu przycisku "Rysuj"
  plot_data <- eventReactive(input$draw, {
    req(nrow(filtered_data()) > 0)
    filtered_data()
  })
  
  # Wykres punktowy z linią trendu
  output$trend_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_scatter_with_trend_plot(plot_data())
  })
  
  # Wykres statystyk
  output$stat_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_stat_plot(
      calculate_statistics(plot_data(), range_of_years),
      input$statistic,
      input$statistic
    )
  })
  
  # Wykres sezonowy
  output$season_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_season_plot(calculate_statistics(plot_data(), season))
  })
  
  # Tau
  output$tau_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_tau_plot(plot_data())
  })
  
  output$tau_result <- renderText({
    req(nrow(plot_data()) > 0)
    tau <- count_tau_kendall(plot_data())
    paste("Współczynnik Tau Kendalla:", round(tau, 2))
  })
  
  # Wykres świecznikowy
  output$candlestick_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_candlestick_plot(plot_data())
  })
  
  # Wykres czasowy
  output$time_series_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_time_series_plot(plot_data())
  })
  
  # Wykres rozrzutu
  output$scatter_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_scatter_plot(plot_data())
  })
  
  # Wykres średnich
  output$mean_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_mean_plot(plot_data(), calculate_mean_results(plot_data()))
  })
  
  # Wykres rozkładu wartości
  output$distribution_plot <- renderPlot({
    req(nrow(plot_data()) > 0)
    draw_value_distribution_plot(plot_data())
  })
}

shinyApp(ui = ui, server = server)

