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
count_tau_kendall <- function(data, zmienna1, zmienna2) {
  cleaned_data <- data %>%
    select(all_of(c(zmienna1, zmienna2))) %>%
    na.omit()
  cleaned_data[[zmienna1]] <- as.numeric(data[[zmienna1]])
  cleaned_data[[zmienna2]] <- as.numeric(data[[zmienna2]])
  tau_kendall <- cor(cleaned_data[[zmienna1]], cleaned_data[[zmienna2]], method = "kendall")

  return(tau_kendall)
}



library(ggplot2)

#Przykładowe wykresy
# Utworzenie histogramu z podziałem na rodzaj
ggplot(combined_df, aes(x = rok)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histogram of Years by Type",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ rodzaj, scales = "free")

library(ggplot2)
library(dplyr)

# Przekształcenie kolumny 'rok' na typ numeric
combined_df <- combined_df %>%
  mutate(rok = as.numeric(as.character(rok)))

combined_df <- combined_df %>%
  mutate(przedzial_lat = cut(rok,
                             breaks = seq(2003, 2024, by = 3),
                             labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
                             right = TRUE))
combined_df = combined_df[-c(631,1260,1470),]
# Utworzenie histogramu z kolorowaniem według zmiennej 'rodzaj' i podziałem na przedziały lat co 3 lata
  ggplot(combined_df, aes(x = przedzial_lat, fill = rodzaj)) +
  geom_histogram(stat = "count", color = "black", position = "dodge") +
  labs(title = "Histogram of Years by Type",
       x = "Year Range",
       y = "Frequency") +
  theme_minimal()





  ggplot(mean_results, aes(x = przedzial_lat, y = srednia_wartosc, group = rodzaj)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "Średnie wyniki w obrębie rodzaju dla poszczególnych przedziałów czasowych",
         x = "Przedział czasowy",
         y = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ rodzaj, scales = "free_y")

  ggplot(mean_results, aes(x = przedzial_lat, y = srednia_wartosc, color = rodzaj, group = rodzaj)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "Średnie wyniki w obrębie rodzaju dla poszczególnych przedziałów czasowych",
         x = "Przedział czasowy",
         y = "Średnia wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ rodzaj, scales = "free_y")


##########################
  #Boxplot
  ggplot(combined_df, aes(x = przedzial_lat, y = wartosc, fill = rodzaj)) +
    geom_boxplot() +
    labs(title = "Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Wartość",
         fill = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Histogram
  ggplot(combined_df, aes(x = wartosc, fill = rodzaj)) +
    geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
    labs(title = "Histogram wartości z podziałem na rodzaje",
         x = "Wartość",
         y = "Liczba wystąpień",
         fill = "Rodzaj") +
    theme_minimal()

#Wykres świecznikowy
  ggplot(combined_df, aes(x = przedzial_lat, y = wartosc, color = rodzaj)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Wykres świecznikowy wartości dla różnych rodzajów",
         x = "Przedział czasowy",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Wykres punktowy z liniami trendu
  ggplot(combined_df, aes(x = rok, y = wartosc, color = rodzaj)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Wykres punktowy z linią trendu dla wartości",
         x = "Rok",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal()

  # wykres czasowy 
  ggplot(combined_df, aes(x = rok, y = wartosc, color = rodzaj, group = rodzaj)) +
    geom_line() +
    geom_point() +
    labs(title = "Wykres czasowy wartości dla różnych rodzajów",
         x = "Rok",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal()

  #Heatmapa
  mean_results <- combined_df %>%
    group_by(przedzial_lat, rodzaj) %>%
    summarize(srednia_wartosc = mean(wartosc, na.rm = TRUE))

  ggplot(mean_results, aes(x = przedzial_lat, y = rodzaj, fill = srednia_wartosc)) +
    geom_tile() +
    labs(title = "Heatmapa średnich wartości dla różnych rodzajów i przedziałów czasowych",
         x = "Przedział czasowy",
         y = "Rodzaj",
         fill = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


#wykres pudełkowy dla różnych rodzajów
  ggplot(combined_df, aes(x = rodzaj, y = wartosc, fill = rodzaj)) +
    geom_boxplot() +
    labs(title = "Porównanie wartości dla różnych rodzajów",
         x = "Rodzaj",
         y = "Wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

  # Wykres Rozrzutu (Scatter Plot) dla różnych przedziałów czasowych
  ggplot(combined_df, aes(x = przedzial_lat, y = wartosc, color = rodzaj)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(title = "Wykres rozrzutu wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FUNKCJA DO FILTROWANIA
library(dplyr)
library(tidyr)

# Zakładamy, że combined_df został wcześniej utworzony zgodnie z poprzednimi instrukcjami
# combined_df <- your_data_frame

filter_and_group <- function(data, start_year, end_year, type = NULL) {
  # Filtrowanie danych na podstawie zakresu lat
  filtered_df <- data %>%
    filter(rok >= start_year & rok <= end_year)
  
  # Opcjonalne filtrowanie na podstawie rodzaju, jeśli został podany
  if (!is.null(type)) {
    filtered_df <- filtered_df %>%
      filter(rodzaj == type)
  }

  Przykład użycia pokazuje, jak można zastosować tę funkcję do ramki danych combined_df, 
  aby przefiltrować dane dla lat od 2010 do 2015 i rodzaju baggy-jeans,
  a następnie grupować i obliczać średnią wartość dla każdej grupy.
