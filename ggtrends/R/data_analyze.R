#' Calculate statistical parameters (mean, median, standard deviation)
#'
#' @param df A data frame containing the data.
#' @param parameter The parameter to group by.
#' @return A data frame with calculated statistics.
#' @importFrom dplyr mutate group_by summarize
#' @export
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
#' @param data A data frame containing `year` and `value` columns.
#' @return The Kendall's Tau coefficient.
#' @importFrom stats cor
#' @export
count_tau_kendall <- function(data) {
  cleaned_data <- na.omit(data[, c("year", "value")])
  tau_kendall <- cor(cleaned_data$year, cleaned_data$value, method = "kendall")
  return(tau_kendall)
}

#' Calculate mean results for the data
#'
#' @param data A data frame containing the data.
#' @return A data frame with mean results.
#' @importFrom dplyr group_by summarize
#' @export
calculate_mean_results <- function(data) {
  mean_results <- data %>%
    group_by(range_of_years, type) %>%
    summarize(mean_value = mean(value, na.rm = TRUE))
  return(mean_results)
}
