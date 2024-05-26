server = function(input, output) {
  output[["summary"]] = renderDT({
    complete
  })
  
  output[["wykres"]] = renderPlot({
    if (input$plotType == "Wykres kołowy - id toru") {
      wykres_kolowy_tor
    } else if (input$plotType == "Wykres kołowy - miesiąc") {
      wykres_kolowy_miesiac
    } else if (input$plotType == "Typy wyścigów konnych") {
      derby_race_types_histogram
    }
  })
  
  
  output[["wykres_1"]] = renderPlot({
    if ( input$plot_type == "top 10 dżokejów") {
      race_type = input$race_type
      
      if (race_type == 'wszystkie'){
        ggplot(ost_top_10, aes(x = jockey, y = sum, fill = race_type)) +
          geom_bar(stat = "identity") +
          labs(title = "Top 10 dżokejów z największą ilością osiągniętych pierwszych miejsc",
               x = "Dżokej",
               y = "Suma",
               fill = "Typ wyścigu") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          theme(legend.position = "bottom") +
          theme(plot.title = element_text(hjust = 0.5))
      } 
      else {
        typ = race_type
        jockey_race_type <- jockey_first_place[race_type == typ][order(-sum)]
        ggplot(jockey_race_type[1:10,], aes(x = jockey, y = sum, fill = jockey)) +
          geom_bar(stat = "identity")  +
          labs(title = paste0("Top 10 dżokejów wyścigu ", race_type, ""),
               x = "Dżokej",
               y = "Suma") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          theme(legend.position = "none") +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
    else if (input$plot_type == "osiągnięcia dżokejów"){
      jockey_name = input$jockey_type
      ggplot(start[start$jockey == jockey_name], aes(x = final_place, fill = track_id)) +
        geom_histogram(bins = 30) +
        labs(title = paste0(jockey_name, " - ", "miejca na poszczególnych torach"),
             x = "Miejsce",
             y = "Ilość wyścigów",
             fill = "Tor") +
        theme_minimal() +
        theme(legend.position = "bottom") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = seq(1, 15, 1)) +
        scale_y_continuous(breaks = c(0, 30, 60, 90, 120)) +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  
}
#output[["tabela"]] = renderDT({
#    
#  })