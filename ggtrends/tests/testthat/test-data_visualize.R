test_that("draw_season_plot function works properly", {
  data <- data.frame(
    season = c("Spring", "Summer", "Autumn", "Winter"),
    mean_value = c(1, 2, 3, 4),
    type = rep("A", 4)
  )
  plot <- draw_season_plot(data, "A")
  expect_true("ggplot" %in% class(plot))
})

test_that("create_plot function works properly", {
  statistics_df <- data.frame(
    range_of_years = c("2004-2006", "2007-2009", "2010-2012"),
    type = c("A", "A", "A"),
    mean_value = c(1, 2, 3)
  )
  plot <- create_plot(statistics_df, "mean_value", "Średnia")
  expect_true("ggplot" %in% class(plot))
})

test_that("draw_trend_line function works properly", {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )
  plot <- draw_trend_line(data, "x", "y")
  expect_true("ggplot" %in% class(plot))
})
