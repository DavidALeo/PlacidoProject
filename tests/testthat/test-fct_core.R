library(PlacidoApp)

test_that("get_sample_sizes returns correct results", {
  expect_equal(get_sample_sizes(300, "first"), 30)
  expect_equal(get_sample_sizes(1500, "first"), 50)
  expect_equal(get_sample_sizes(4000, "second"), 80)
})

test_that("get_sample_sizes returns error for invalid batch size", {
  expect_error(get_sample_sizes(99, "first"), "Batch size is outside the range of possible values.")
})

test_that("get_sample_sizes returns error for invalid analysis type", {
  expect_error(get_sample_sizes(300, "third"), "Analysis parameter must be either 'first' 'second' or 'mean'.")
})

test_that("noncon_thres_value returns correct results", {

  # Test a nominal quantity within the reference table range
  expect_equal(noncon_thres_value(20), 18.2)

  # Test a nominal quantity with an absolute value for the maximum error tolerance
  expect_equal(noncon_thres_value(250), 241)
})

test_that("noncon_thres_value returns error for nominal quantities on limit cases", {
  # Test nominal quantity at the lower bound
  expect_error(noncon_thres_value(4), "Nominal quantity is outside the range of the reference table.")
  # Test nominal quantity at the upper bound
  expect_error(noncon_thres_value(10001), "Nominal quantity is outside the range of the reference table.")
})

test_that("noncon_limit_value returns correct results", {

  # Test a nominal quantity within the reference table range
  expect_equal(noncon_limit_value(20), 16.4)

  # Test a nominal quantity with an absolute value for the maximum error tolerance
  expect_equal(noncon_limit_value(250), 232)
})

test_that("noncon_limit_value returns error for nominal quantities on the edge of the range", {

  # Test a nominal quantity at the lower edge of the range
  expect_error(noncon_limit_value(4), "Nominal quantity is outside the range of the reference table.")

  # Test a nominal quantity at the upper edge of the range
  expect_error(noncon_limit_value(10001), "Nominal quantity is outside the range of the reference table.")
})


test_that("first_noncon_analysis works correctly for normal input", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Acceptable input
  df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 99, 91)))
  expected_df <- data.frame(status = c(rep("Aceptable", 29),c("Non-conformity")))
  result <- first_noncon_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Accept")
  expect_equal(result$df$status, expected_df$status)

  # Second analysis input
  df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 91, 91)))
  expected_df <- data.frame(status = c(rep("Aceptable", 28),rep("Non-conformity", 2)))
  result <- first_noncon_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Second analysis")
  expect_equal(result$df$status, expected_df$status)

  # Too many non-conformities
  df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 91, 91, 91)))
  expected_df <- data.frame(status = c(rep("Aceptable", 27),rep("Non-conformity", 3)))
  result <- first_noncon_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Reject")
  expect_equal(result$df$status, expected_df$status)

  # Rejection value
  df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 99, 90)))
  expected_df <- data.frame(status = c(rep("Aceptable", 29),rep("Rejection", 1)))
  result <- first_noncon_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Reject")
  expect_equal(result$df$status, expected_df$status)
})

test_that("first_noncon_analysis don't accept incorrect sample sizes", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Sample too small
  df <- data.frame(value = rep(nominal_quantity, 29))
  expect_error(
    first_noncon_analysis(df, col_name, nominal_quantity, batch_size),
    "The number of rows in the input dataframe does not match the required sample size."
  )

  # Sample too big
  df <- data.frame(value = rep(nominal_quantity, 31))
  expect_error(
    first_noncon_analysis(df, col_name, nominal_quantity, batch_size),
    "The number of rows in the input dataframe does not match the required sample size."
  )
})

test_that("first_noncon_analysis don't accept incorrect col_name argument", {
  col_name <- "other_value"
  nominal_quantity <- 100
  batch_size <- 100

  df <- data.frame(value = rep(nominal_quantity, 30))
  expect_error(
    first_noncon_analysis(df, col_name, nominal_quantity, batch_size),
    "The col_name argument must be a column index value of the input dataframe."
  )

})

test_that("second_noncon_analysis works correctly for normal input", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Acceptable input
  first_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 91, 91)))
  second_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 91, 91)))
  expected_first_df <- data.frame(status = c(rep("Aceptable", 28),rep("Non-conformity",2)))
  expected_second_df <- data.frame(status = c(rep("Aceptable", 28),rep("Non-conformity",2)))
  result <- second_noncon_analysis(first_df, col_name, second_df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Accept")
  expect_equal(result$first_analysis_df$status, expected_first_df$status)
  expect_equal(result$second_analysis_df$status, expected_second_df$status)

  # Too many non-conformities
  first_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 91, 91)))
  second_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 91, 91, 91)))
  expected_first_df <- data.frame(status = c(rep("Aceptable", 28),rep("Non-conformity",2)))
  expected_second_df <- data.frame(status = c(rep("Aceptable", 27),rep("Non-conformity",3)))
  result <- second_noncon_analysis(first_df, col_name, second_df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Reject")
  expect_equal(result$first_analysis_df$status, expected_first_df$status)
  expect_equal(result$second_analysis_df$status, expected_second_df$status)

  # Rejection value
  first_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 91, 91)))
  second_df <- data.frame(value = c(rep(nominal_quantity, 25),c(99, 99, 99, 99, 90)))
  expected_first_df <- data.frame(status = c(rep("Aceptable", 28),rep("Non-conformity",2)))
  expected_second_df <- data.frame(status = c(rep("Aceptable", 29),rep("Rejection",1)))
  result <- second_noncon_analysis(first_df, col_name, second_df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Reject")
  expect_equal(result$first_analysis_df$status, expected_first_df$status)
  expect_equal(result$second_analysis_df$status, expected_second_df$status)

})

test_that("second_noncon_analysis don't accept incorrect sample sizes", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Sample too small
  first_df <- data.frame(value = rep(nominal_quantity, 30))
  second_df <- data.frame(value = rep(nominal_quantity, 29))
  expect_error(
    second_noncon_analysis(first_df, col_name, second_df, col_name, nominal_quantity, batch_size),
    "The number of rows in the input dataframe does not match the required sample size."
  )

  # Sample too big
  first_df <- data.frame(value = rep(nominal_quantity, 30))
  second_df <- data.frame(value = rep(nominal_quantity, 31))
  expect_error(
    second_noncon_analysis(first_df, col_name, second_df, col_name, nominal_quantity, batch_size),
    "The number of rows in the input dataframe does not match the required sample size."
  )
})

test_that("first_noncon_analysis don't accept incorrect col_name argument", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # First sample
  first_df <- data.frame(value = rep(nominal_quantity, 30))
  second_df <- data.frame(value = rep(nominal_quantity, 30))
  expect_error(
    second_noncon_analysis(first_df, "col_name_other", second_df, col_name, nominal_quantity, batch_size),
    "The col_name argument must be a column index value of the input dataframe."
  )

  # Second sample
  first_df <- data.frame(value = rep(nominal_quantity, 30))
  second_df <- data.frame(value = rep(nominal_quantity, 30))
  expect_error(
    second_noncon_analysis(first_df, col_name, second_df, "col_name_other", nominal_quantity, batch_size),
    "The col_name argument must be a column index value of the input dataframe."
  )
})

test_that("std_estimation yields expected results", {
  # Test case 1
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  col_name <- "x"
  expected_result <- sqrt(10/4)
  result <- std_dev_estimate(df, col_name)
  expect_equal(result, expected_result)

  # Test case 2
  df <- data.frame(x = c(0, 0, 0, 0, 0))
  col_name <- "x"
  expected_result <- 0
  result <- std_dev_estimate(df, col_name)
  expect_equal(result, expected_result)

  # Test case 3
  df <- data.frame(x = c(1.5, 2.5, 3.5, 4.5, 5.5))
  col_name <- "x"
  expected_result <- sqrt(10/4)
  result <- std_dev_estimate(df, col_name)
  expect_equal(result, expected_result)
})

test_that("mean_analysis works correctly for normal input", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Accept input
  df <- data.frame(value = rep(nominal_quantity, 30))
  result <- mean_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Accept")

  # Rejection value
  df <- data.frame(value = rep(nominal_quantity-10, 30))
  result <- mean_analysis(df, col_name, nominal_quantity, batch_size)
  expect_equal(result$decision, "Reject")
})

test_that("mean_analysis don't accept incorrect sample sizes", {
  col_name <- "value"
  nominal_quantity <- 100
  batch_size <- 100

  # Sample too small
  df <- data.frame(value = rep(nominal_quantity, 29))
  expect_error(
    mean_analysis(df, col_name, nominal_quantity, batch_size),
    "The number of rows in the input dataframe does not match the required sample size."
  )
})

test_that("mean_analysis don't accept incorrect col_name argument", {
  col_name <- "other_value"
  nominal_quantity <- 100
  batch_size <- 100

  df <- data.frame(value = rep(nominal_quantity, 30))
  expect_error(
    mean_analysis(df, col_name, nominal_quantity, batch_size),
    "The col_name argument must be a column index value of the input dataframe."
  )

})
