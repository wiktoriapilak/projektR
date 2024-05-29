library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Analiza google trends"),

  tabsetPanel(
    tabPanel("Parametry statystyczne",
             sidebarLayout(
               sidebarPanel(
                 selectInput("statistic", "Wybierz statystykę:",
                             choices = list("Średnie" = "mean_value", "Median" = "median_value", "Odchylenie standardowe" = "sd_value")),
                 checkboxGroupInput("types", "Wybierz typy:",
                                    choices = unique(combined_df$type),
                                    selected = unique(combined_df$type)),
                 actionButton("select_all", "Zaznacz wszystkie"),
                 actionButton("deselect_all", "Odznacz wszystkie")
               ),
               mainPanel(
                 plotOutput("statPlot")
               )
             )
    ),

    tabPanel("Wykres sezonowy",
             sidebarLayout(
               sidebarPanel(
                 selectInput("season_type", "Wybierz typ dla wykresu sezonowego:",
                             choices = unique(combined_df$type))
               ),
               mainPanel(
                 plotOutput("seasonPlot")
               )
             )
    ),

    tabPanel("Wykres punktowy z linią trendu",
             sidebarLayout(
               sidebarPanel(
                 selectInput("zmienna_x", "Wybierz zmienną dla osi X:",
                             choices = names(combined_df)),
                 selectInput("zmienna_y", "Wybierz zmienną dla osi Y:",
                             choices = names(combined_df))
               ),
               mainPanel(
                 plotOutput("trendPlot"),
                 textOutput("tauResult")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "types", selected = unique(combined_df$type))
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "types", selected = character(0))
  })

  filtered_data <- reactive({
    if (is.null(input$types) || length(input$types) == 0) {
      combined_df
    } else {
      combined_df %>%
        filter(type %in% input$types)
    }
  })

  statistics <- reactive({
    calculate_statistics(filtered_data(), range_of_years)
  })

  output$statPlot <- renderPlot({
    create_plot(statistics(), input$statistic, input$statistic)
  })

  output$seasonPlot <- renderPlot({
    season_stats <- calculate_statistics(combined_df, season)
    draw_season_plot(season_stats, input$season_type)
  })

  output$trendPlot <- renderPlot({
    p <- draw_trend_line(combined_df, input$zmienna_x, input$zmienna_y)
    tau_value <- oblicz_tau_kendalla(combined_df, input$zmienna_x, input$zmienna_y)
    tau_text <- paste("Współczynnik korelacji Tau Kendalla:", tau_value)
    output$tauResult <- renderText({
      tau_text
    })
    return(p)
  })
}

shinyApp(ui = ui, server = server)
