test_that("calculate_statistics function works properly", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5),
    type = c("A", "A", "B", "B", "B"),
    parameter = c("X", "X", "Y", "Y", "Y")
  )
  statistics <- calculate_statistics(df, "parameter")
  expect_equal(nrow(statistics), 2)
  expect_equal(statistics$parameter, c("X", "Y"))
  expect_equal(statistics$type, c("A", "B"))
  expect_equal(statistics$mean_value, c(1.5, 4))
  expect_equal(statistics$median_value, c(1.5, 4))
  expect_equal(statistics$n, c(2, 3))
})

test_that("count_tau_kendall function works properly", {
  data <- data.frame(
    year = c(1, 2, 3, 4, 5),
    value = c(5, 4, 3, 2, 1)
  )
  tau_kendall <- count_tau_kendall(data)
  expect_equal(tau_kendall, -1)
})

test_that("calculate_mean_results function works properly", {
  data <- data.frame(
    value = c(1, 2, 3, 4, 5),
    range_of_years = c("2004-2006", "2004-2006", "2007-2009", "2007-2009", "2007-2009"),
    type = c("A", "A", "B", "B", "B")
  )
  mean_results <- calculate_mean_results(data)
  expect_equal(nrow(mean_results), 2)
  expect_equal(mean_results$range_of_years, c("2004-2006", "2007-2009"))
  expect_equal(mean_results$type, c("A", "B"))
  expect_equal(mean_results$mean_value, c(1.5, 4))
})
