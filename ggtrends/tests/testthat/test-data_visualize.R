test_that("draw_season_plot function works properly", {
  data <- data.frame(
    season = c("Spring", "Summer", "Autumn", "Winter"),
    mean_value = c(1, 2, 3, 4),
    type = rep("A", 4)
  )
  plot <- draw_season_plot(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("create_stat_plot function works properly", {
  statistics_df <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    type = c("A", "A", "A"),
    mean_value = c(1, 2, 3)
  )
  plot <- create_stat_plot(statistics_df, "mean_value", "Średnia")
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_tao function works properly", {
  data <- data.frame(
    year = c(1, 2, 3, 4, 5),
    value = c(2, 4, 6, 8, 10)
  )
  plot <- draw_tao(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_candlestick_plot function works properly", {
  data <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    value = c(1, 2, 3),
    type = c("A", "A", "A")
  )
  plot <- draw_candlestick_plot(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_scatter_with_trend_plot function works properly", {
  data <- data.frame(
    year = c(1, 2, 3, 4, 5),
    value = c(2, 4, 6, 8, 10),
    type = c("A", "A", "B", "B", "B")
  )
  plot <- draw_scatter_with_trend_plot(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_time_series_plot function works properly", {
  data <- data.frame(
    year = c(2004, 2005, 2006, 2007, 2008),
    value = c(1, 2, 3, 4, 5),
    type = c("A", "A", "B", "B", "B")
  )
  plot <- draw_time_series_plot(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_scatter_plot function works properly", {
  data <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    value = c(1, 2, 3),
    type = c("A", "A", "A")
  )
  plot <- draw_scatter_plot(data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_mean_plot function works properly", {
  data <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    mean_value = c(1, 2, 3),
    type = c("A", "A", "A")
  )
  plot <- draw_mean_plot(data, data)
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_value_distribution_plot function works properly", {
  data <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    value = c(1, 2, 3),
    type = c("A", "A", "A")
  )
  plot <- draw_value_distribution_plot(data)
  expect_true("ggplot" %in% class(plot))
})
