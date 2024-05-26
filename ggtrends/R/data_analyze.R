# Analiza statystyczna tj. mediana, średnia, odchylenie standardowe
calculate_statistics <- function(df,parameter) {
  statistics <- df %>%
    mutate(value = as.numeric(value)) %>%  # Konwersja kolumny 'value' na typ numeryczny
    group_by({{ parameter }}, value) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      median_value = median(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup()
  statistics <- statistics[-c(31:33),]
  return(statistics)
}

# Funkcja do obliczania współczynnika Tau Kendalla dla dowolnych dwóch zmiennych
oblicz_tau_kendalla <- function(data, zmienna1, zmienna2) {
  # Usunięcie brakujących wartości
  cleaned_data <- data %>%
    select(all_of(c(zmienna1, zmienna2))) %>%
    na.omit()
  cleaned_data[[zmienna1]] <- as.numeric(data[[zmienna1]])
  cleaned_data[[zmienna2]] <- as.numeric(data[[zmienna2]])

  # Obliczenie Tau Kendalla
  tau_kendall <- cor(cleaned_data[[zmienna1]], cleaned_data[[zmienna2]], method = "kendall")

  return(tau_kendall)
}

# Przykład użycia funkcji
tau_kendall_wartosc_rok <- oblicz_tau_kendalla(combined_df, "value", "year")
print(paste("Współczynnik korelacji Tau Kendalla między 'wartosc' a 'rok':", tau_kendall_wartosc_rok))
