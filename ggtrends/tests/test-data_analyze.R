# Analiza sezonowa
combined_df <- combined_df %>%
    mutate(miesiac = as.numeric(miesiac)) %>%  # Konwersja kolumny 'miesiac' na typ numeryczny
    mutate(sezon = case_when(
      miesiac %in% c(12, 1, 2) ~ "Zima",
      miesiac %in% c(3, 4, 5) ~ "Wiosna",
      miesiac %in% c(6, 7, 8) ~ "Lato",
      miesiac %in% c(9, 10, 11) ~ "Jesień"
    ))
  
  statystyki_sezonowe <- combined_df %>%
    group_by(sezon, rodzaj) %>%
    summarize(
      srednia = mean(wartosc, na.rm = TRUE),
      mediana = median(wartosc, na.rm = TRUE),
      odchylenie_standardowe = sd(wartosc, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup()

  # Funkcja do tworzenia wykresów dla poszczególnych rodzajów
  tworz_wykres_sezonowy <- function(data, rodzaj) {
    filtered_data <- data %>% filter(rodzaj == !!rodzaj)
    
    ggplot(filtered_data, aes(x = sezon, y = srednia, fill = sezon)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Średnia wartość dla rodzaju", rodzaj, "w poszczególnych sezonach"),
           x = "Sezon",
           y = "Średnia wartość") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none")
  }
  # Przykład użycia funkcji
  rodzaj <- "baggy_jeans"
  tworz_wykres_sezonowy(statystyki_sezonowe, rodzaj)
