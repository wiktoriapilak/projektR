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

# Analiza statystyczna tj. mediana, średnia, odchylenie standardowe
statystyki_opisowe <- combined_df %>%
    group_by(przedzial_lat, rodzaj) %>%
    summarize(
      srednia = mean(wartosc, na.rm = TRUE),
      mediana = median(wartosc, na.rm = TRUE),
      odchylenie_standardowe = sd(wartosc, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup()
  statystyki_opisowe = statystyki_opisowe[-c(31:33),]
  
# Wykres średnich
  ggplot(statystyki_opisowe, aes(x = przedzial_lat, y = srednia, color = rodzaj, group = rodzaj)) +
    geom_line() +
    geom_point() +
    labs(title = "Średnie wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  # Wykres median
  ggplot(statystyki_opisowe, aes(x = przedzial_lat, y = mediana, color = rodzaj, group = rodzaj)) +
    geom_line() +
    geom_point() +
    labs(title = "Median wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Mediana wartości") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  # Wykres odchylenia standardowego
  ggplot(statystyki_opisowe, aes(x = przedzial_lat, y = odchylenie_standardowe, color = rodzaj, group = rodzaj)) +
    geom_line() +
    geom_point() +
    labs(title = "Odchylenie standardowe wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Odchylenie standardowe wartości") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

    
# funkcja usprawniająca bardziej do shiny
  create_plot <- function(statistics_df, stat_col, stat_name) {
    ggplot(statistics_df, aes_string(x = "przedzial_lat", y = stat_col, color = "rodzaj", group = "rodzaj")) +
      geom_line() +
      geom_point() +
      labs(title = paste(stat_name, "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"),
           x = "Przedział czasowy",
           y = paste(stat_name, "wartość")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Przykładowe wywołania funkcji dla różnych statystyk
  create_plot(statystyki_opisowe, "srednia", "Średnie")
  create_plot(statystyki_opisowe, "mediana", "Median")
  create_plot(statystyki_opisowe, "odchylenie_standardowe", "Odchylenie standardowe")
  
