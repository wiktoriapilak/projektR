# Analiza statystyczna tj. mediana, średnia, odchylenie standardowe
calculate_statistics <- function(df, parameter) {
  statistics <- df %>%
    mutate(value = as.numeric(value)) %>%
    group_by({{ parameter }}, type) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      median_value = median(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  return(statistics)
}

# Funkcja do obliczania współczynnika Tau Kendalla dla dowolnych dwóch zmiennych
oblicz_tau_kendalla <- function(data, zmienna1, zmienna2) {
  cleaned_data <- data %>%
    select(all_of(c(zmienna1, zmienna2))) %>%
    na.omit()
  cleaned_data[[zmienna1]] <- as.numeric(data[[zmienna1]])
  cleaned_data[[zmienna2]] <- as.numeric(data[[zmienna2]])
  tau_kendall <- cor(cleaned_data[[zmienna1]], cleaned_data[[zmienna2]], method = "kendall")

  return(tau_kendall)
}


