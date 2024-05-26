library(ggplot2)

# 1. Tworzenie wykresu sezonowego
draw_season_plot <- function(data, type) {
  filtered_data <- data %>% filter(type == !!type)

  ggplot(filtered_data, aes(x = season, y = mean_value, fill = season)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Średnia wartość dla rodzaju", type, "w poszczególnych sezonach"),
         x = "Sezon",
         y = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")
}

# Przykład użycia funkcji
value <- "baggy_jeans"
draw_season_plot(calculate_statistics(combined_df,season), value)



# 2. Funkcja do rysowania wykresow dla statstyk
statystyki_opisowe=calculate_statistics(combined_df,range_of_years)

create_plot <- function(statistics_df, stat_col, stat_name) {
  ggplot(statistics_df, aes_string(x = "range_of_years", y = stat_col, color = "type", group = "type")) +
    geom_line() +
    geom_point() +
    labs(title = paste(stat_name, "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"),
         x = "Przedział czasowy",
         y = paste(stat_name, "wartość")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Przykładowe wywołania funkcji dla różnych statystyk
create_plot(statystyki_opisowe, "mean_value", "Średnie")
create_plot(statystyki_opisowe, "median_value", "Median")
create_plot(statystyki_opisowe, "sd_value", "Odchylenie standardowe")




# 3. Funkcja do tworzenia wykresów punktowych z linią trendu
draw_trend_line <- function(data, zmienna_x, zmienna_y) {
  cleaned_data <- data %>%
    na.omit() %>%
    select(all_of(c(zmienna_x, zmienna_y)))

  ggplot(cleaned_data, aes_string(x = zmienna_x, y = zmienna_y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Zależność między", zmienna_x, "a", zmienna_y),
         x = zmienna_x,
         y = zmienna_y) +
    theme_minimal()
}

# Przykład użycia funkcji
draw_trend_line(combined_df, "year", "value")
