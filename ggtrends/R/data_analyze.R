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

# Funkcja do obliczania współczynnika Tau Kendalla dla dwóch zmiennych
count_tau_kendall <- function(data) {
  cleaned_data <- na.omit(data[, c("year", "value")])
  tau_kendall <- cor(cleaned_data$year, cleaned_data$value, method = "kendall")
  return(tau_kendall)
}

# funkcja pomocnicza do sredniej wartosci
calculate_mean_results <- function(data) {
  mean_results <- data %>%
    group_by(range_of_years, type) %>%
    summarize(mean_value = mean(value, na.rm = TRUE))
  return(mean_results)
}

