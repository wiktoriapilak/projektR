library(ggplot2)

# Tworzenie wykresu sezonowego
draw_season_plot <- function(data) {
  ggplot(data, aes(x = season, y = mean_value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Średnia wartość w poszczególnych sezonach",
         x = "Sezon",
         y = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Funkcje do rysowania wykresów dla statystyk
create_stat_plot <- function(statistics_df, stat_col, stat_name) {
  ggplot(statistics_df, aes_string(x = "range_of_years", y = stat_col, color = "type", group = "type")) +
    geom_line() +
    geom_point() +
    labs(title = paste(stat_name, "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"),
         x = "Przedział czasowy",
         y = paste(stat_name, "wartość")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Funkcja do tao kendalla
draw_tao <- function(data) {
  cleaned_data <- na.omit(data[, c("year", "value")])

  ggplot(cleaned_data, aes(x = year, y = value)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Zależność między rokiem a wartością",
         x = "Rok",
         y = "Wartość") +
    theme_minimal()
}

#wykres swiecznikowy
draw_candlestick_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, color = type)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Wykres świecznikowy wartości dla różnych rodzajów",
         x = "Przedział czasowy",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Wykres punktowy z linią trendu dla wartości
draw_scatter_with_trend_plot <- function(data) {
  ggplot(data, aes(x = year, y = value, color = type)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Wykres punktowy z linią trendu dla wartości",
         x = "Rok",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal()
}

# Wykres czasowy wartości dla różnych rodzajów
draw_time_series_plot <- function(data) {
  ggplot(data, aes(x = year, y = value, color = type, group = type)) +
    geom_line() +
    geom_point() +
    labs(title = "Wykres czasowy wartości dla różnych rodzajów",
         x = "Rok",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal()
}

#Wykres rozrzutu wartości dla różnych rodzajów w poszczególnych przedziałach czasowych
draw_catter_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, color = type)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(title = "Wykres rozrzutu wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Średnie wyniki w obrębie rodzaju dla poszczególnych przedziałów czasowych
draw_mean_plot<-function(data,mean_results){
ggplot(mean_results, aes(x = range_of_years, y = mean_value, color = type, group = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Średnie wyniki w obrębie rodzaju dla poszczególnych przedziałów czasowych",
       x = "Przedział czasowy",
       y = "Średnia wartość",
       color = "Rodzaj") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ type, scales = "free_y")
}

#Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych
draw_value_distribution_plot<-function(data){
ggplot(data, aes(x = range_of_years, y = value, fill = type)) +
  geom_boxplot() +
  labs(title = "Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
       x = "Przedział czasowy",
       y = "Wartość",
       fill = "Rodzaj") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
