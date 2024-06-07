#' Calculate statistical parameters (mean, median, standard deviation)
#'
#' This function calculates statistical parameters (mean, median, standard deviation) for each group defined by the given parameter and `type`.
#'
#' @param df A data frame containing the data.
#' @param parameter The column name to group by (unquoted).
#' @return A data frame with calculated statistics.
#' @importFrom dplyr mutate group_by summarize
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(type = rep(c("A", "B"), each = 5), value = rnorm(10), year = rep(2001:2005, 2))
#' calculate_statistics(data, year)
#' }
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

#' Calculate Kendall's Tau coefficient for two variables
#'
#' This function calculates Kendall's Tau coefficient for the correlation between `year` and `value` columns.
#'
#' @param data A data frame containing `year` and `value` columns.
#' @return The Kendall's Tau coefficient.
#' @importFrom stats cor
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(year = rep(2001:2005, each = 2), value = rnorm(10))
#' count_tau_kendall(data)
#' }
count_tau_kendall <- function(data) {
  cleaned_data <- na.omit(data[, c("year", "value")])
  tau_kendall <- cor(cleaned_data$year, cleaned_data$value, method = "kendall")
  return(tau_kendall)
}

#' Calculate mean results for the data
#'
#' This function calculates the mean values of the data grouped by `range_of_years` and `type`.
#'
#' @param data A data frame containing the data.
#' @return A data frame with mean results.
#' @importFrom dplyr group_by summarize
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5), type = rep(c("A", "B"), each = 5), value = rnorm(10))
#' calculate_mean_results(data)
#' }
calculate_mean_results <- function(data) {
  mean_results <- data %>%
    group_by(range_of_years, type) %>%
    summarize(mean_value = mean(value, na.rm = TRUE))
  return(mean_results)
}

