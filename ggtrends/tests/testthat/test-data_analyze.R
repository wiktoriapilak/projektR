test_that("calculate_statistics function works properly", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5),
    type = c("A", "A", "B", "B", "B"),
    parameter = c("X", "X", "Y", "Y", "Y")
  )
  statistics <- calculate_statistics(df, parameter)
  expect_equal(nrow(statistics), 4)
  expect_equal(statistics$parameter, c("X", "X", "Y", "Y"))
  expect_equal(statistics$type, c("A", "B", "A", "B"))
  expect_equal(statistics$mean_value, c(1.5, 4, 1.5, 4))
  expect_equal(statistics$median_value, c(1.5, 4, 1.5, 4))
  expect_equal(statistics$sd_value, c(0.7071068, 0, 0.7071068, 0))
  expect_equal(statistics$n, c(2, 3, 2, 3))
})

test_that("count_tau_kendall function works properly", {
  data <- data.frame(
    X = c(1, 2, 3, 4, 5),
    Y = c(5, 4, 3, 2, 1)
  )
  tau_kendall <- count_tau_kendall(data, "X", "Y")
  expect_equal(tau_kendall, -1)
})
