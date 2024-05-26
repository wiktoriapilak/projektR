ui = fluidPage(
  tabsetPanel(
    tabPanel("Dane",
             dataTableOutput("summary")),
    tabPanel("Wykresy",
             selectInput("plotType", "Wybierz typ wykresu:", 
                         choices = c("Wykres kołowy - id toru", "Wykres kołowy - miesiąc",
                                     "Typy wyścigów konnych")),
             plotOutput("wykres")),
    tabPanel("Wykresy - zmienne",
             selectInput("plot_type", "Wybierz rodzaj wykresu:", 
                         choices = c("top 10 dżokejów",
                                     "osiągnięcia dżokejów")),
             selectInput("race_type", "Wybierz rodzaj wyścigu:", 
                         choices = race_type),
             selectInput("jockey_type", "Wybierz dżokeja:", 
                         choices = jockey_type),
             plotOutput("wykres_1")),
   
  )
)

 #tabPanel("Tabelki",
     #        selectInput("",
      #                   choices = c()),
       #      dataTableOutput("tabela"))