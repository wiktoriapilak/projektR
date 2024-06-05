#' Draw a scatter plot of values for different types across time ranges
#'
#' @param data A data frame containing the data.
#' @return A ggplot object showing the scatter plot.
#' @import ggplot2
#' @export
draw_scatter_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, color = type)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(title = "Wykres rozrzutu wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Wartość",
         color = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Draw a plot of average values within types for different time ranges
#'
#' @param data A data frame containing the original data.
#' @param mean_results A data frame containing the mean results.
#' @return A ggplot object showing the mean values plot.
#' @import ggplot2
#' @export
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

#' Draw a distribution plot of values for different types across time ranges
#'
#' @param data A data frame containing the data.
#' @return A ggplot object showing the value distribution plot.
#' @import ggplot2
#' @export
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

