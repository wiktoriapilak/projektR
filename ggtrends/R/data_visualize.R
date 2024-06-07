#' Draw a seasonal plot
#'
#' This function creates a bar plot showing the average values for different seasons and types.
#'
#' @param data A data frame containing `season`, `mean_value`, and `type` columns.
#' @return A ggplot object showing the seasonal plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(season = rep(c("Winter", "Spring", "Summer", "Autumn"), each = 2),
#'                    mean_value = runif(8), type = rep(c("A", "B"), 4))
#' draw_season_plot(data)
#' }
draw_season_plot <- function(data) {
  ggplot(data, aes(x = season, y = mean_value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Średnia wartość w poszczególnych sezonach", x = "Sezon", y = "Średnia wartość") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create a statistical plot
#'
#' This function creates a line plot showing the given statistical parameter across different time ranges and types.
#'
#' @param statistics_df A data frame with statistical data.
#' @param stat_col The column name for the statistic to plot (unquoted).
#' @param stat_name The name of the statistic to display in the plot title.
#' @return A ggplot object showing the statistical plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' statistics_df <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 2),
#'                             type = rep(c("A", "B"), 2), mean_value = runif(4))
#' draw_stat_plot(statistics_df, mean_value, "Średnia")
#' }
draw_stat_plot <- function(statistics_df, stat_col, stat_name) {
  ggplot(
    statistics_df,
    aes_string(
      x = "range_of_years",
      y = stat_col,
      color = "type",
      group = "type"
    )
  ) +
    geom_line() +
    geom_point() +
    labs(
      title = paste(
        stat_name,
        "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"
      ),
      x = "Przedział czasowy",
      y = paste(stat_name, "wartość")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Draw a Tau plot
#'
#' This function creates a scatter plot showing the relationship between `year` and `value` with a linear trend line.
#'
#' @param data A data frame containing `year` and `value` columns.
#' @return A ggplot object showing the Tau plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(year = rep(2001:2005, each = 2), value = rnorm(10))
#' draw_tau_plot(data)
#' }
draw_tau_plot <- function(data) {
  cleaned_data <- na.omit(data[, c("year", "value")])

  ggplot(cleaned_data, aes(x = year, y = value)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Zależność między rokiem a wartością", x = "Rok", y = "Wartość") +
    theme_minimal()
}

#' Draw a candlestick plot
#'
#' This function creates a candlestick plot showing the value distribution across different time ranges and types.
#'
#' @param data A data frame containing `range_of_years`, `value`, and `type` columns.
#' @return A ggplot object showing the candlestick plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5),
#'                    value = rnorm(10), type = rep(c("A", "B"), 5))
#' draw_candlestick_plot(data)
#' }
draw_candlestick_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, color = type)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
      title = "Wykres świecznikowy wartości dla różnych rodzajów",
      x = "Przedział czasowy",
      y = "Wartość",
      color = "Rodzaj"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Draw a scatter plot with a trend line for values
#'
#' This function creates a scatter plot with a linear trend line showing the values over the years for different types.
#'
#' @param data A data frame containing `year`, `value`, and `type` columns.
#' @return A ggplot object showing the scatter plot with a trend line.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(year = rep(2001:2005, each = 2), value = rnorm(10), type = rep(c("A", "B"), 5))
#' draw_scatter_with_trend_plot(data)
#' }
draw_scatter_with_trend_plot <- function(data) {
  ggplot(data, aes(x = year, y = value, color = type)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Wykres punktowy z linią trendu dla wartości",
      x = "Rok",
      y = "Wartość",
      color = "Rodzaj"
    ) +
    theme_minimal()
}

#' Draw a time series plot of values for different types
#'
#' This function creates a time series plot showing the values over the years for different types.
#'
#' @param data A data frame containing `year`, `value`, and `type` columns.
#' @return A ggplot object showing the time series plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(year = rep(2001:2005, each = 2), value = rnorm(10), type = rep(c("A", "B"), 5))
#' draw_time_series_plot(data)
#' }
draw_time_series_plot <- function(data) {
  ggplot(data, aes(
    x = year,
    y = value,
    color = type,
    group = type
  )) +
    geom_line() +
    geom_point() +
    labs(
      title = "Wykres czasowy wartości dla różnych rodzajów",
      x = "Rok",
      y = "Wartość",
      color = "Rodzaj"
    ) +
    theme_minimal()
}

#' Draw a scatter plot of values for different types across time ranges
#'
#' This function creates a scatter plot showing the values for different types across different time ranges.
#'
#' @param data A data frame containing `range_of_years`, `value`, and `type` columns.
#' @return A ggplot object showing the scatter plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5),
#'                    value = rnorm(10), type = rep(c("A", "B"), 5))
#' draw_scatter_plot(data)
#' }
draw_scatter_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, color = type)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(
      title = "Wykres rozrzutu wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
      x = "Przedział czasowy",
      y = "Wartość",
      color = "Rodzaj"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Draw a plot of average values within types for different time ranges
#'
#' This function creates a line plot showing the average values within types for different time ranges.
#'
#' @param data A data frame containing the original data.
#' @param mean_results A data frame containing the mean results.
#' @return A ggplot object showing the mean values plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5),
#'                    value = rnorm(10), type = rep(c("A", "B"), 5))
#' mean_results <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 2),
#'                            mean_value = runif(4), type = rep(c("A", "B"), 2))
#' draw_mean_plot(data, mean_results)
#' }
#'
draw_mean_plot <- function(data, mean_results) {
  ggplot(mean_results,
         aes(
           x = range_of_years,
           y = mean_value,
           color = type,
           group = type
         )) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Średnie wyniki w obrębie rodzaju dla poszczególnych przedziałów czasowych",
      x = "Przedział czasowy",
      y = "Średnia wartość",
      color = "Rodzaj"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap( ~ type, scales = "free_y")
}

#' Draw a distribution plot of values for different types across time ranges
#'
#' This function creates a box plot showing the distribution of values for different types across different time ranges.
#'
#' @param data A data frame containing `range_of_years`, `value`, and `type` columns.
#' @return A ggplot object showing the value distribution plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5),
#'                    value = rnorm(10), type = rep(c("A", "B"), 5))
#' draw_value_distribution_plot(data)
#' }
#'
draw_value_distribution_plot <- function(data) {
  ggplot(data, aes(x = range_of_years, y = value, fill = type)) +
    geom_boxplot() +
    labs(
      title = "Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
      x = "Przedział czasowy",
      y = "Wartość",
      fill = "Rodzaj"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

