#' Get Sample Sizes
#'
#' Calculates the required sample size for a given batch size and analysis type, based
#' on a reference table of sample sizes.
#'
#' @param batch_size A numeric value indicating the batch size for which the sample size
#' is to be calculated.
#'
#' @param analysis A string value indicating the type of analysis for which the sample size
#' is to be calculated. Must be either "first" or "second".
#'
#' @return A numeric value indicating the required sample size for the given batch size
#' and analysis type. If the batch size is outside the range of the reference table, or if
#' the analysis parameter is not valid, the function returns an error message.
#'
#' @export
#'
#' @examples
#' get_sample_sizes(300, "first")
#' get_sample_sizes(1500, "second")
get_sample_sizes <- function(batch_size, analysis) {
  if (!analysis %in% c("first", "second", "mean")) {
    stop("Analysis parameter must be either 'first' 'second' or 'mean'.")
  }

  sample_col <- paste0(analysis, "_analysis_sample")

  limits <- sample_sizes %>%
    filter(batch_size >= .data$batch_size_min, batch_size <= .data$batch_size_max) %>%
    pull(sample_col)

  if(length(limits) > 0) {
    limits %>% as.numeric
  } else {
    stop("Batch size is outside the range of possible values.")
  }
}


#' Nonconformity Threshold Value
#'
#' Calculates the maximum error tolerance for a given nominal quantity, based on a
#' reference table of maximum errors.
#'
#' @param nominal_quantity A numeric value indicating the nominal quantity for which
#' the maximum error tolerance is to be calculated.
#'
#' @return A numeric value indicating the maximum error tolerance for the given
#' nominal quantity. If the nominal quantity is outside the range of the reference
#' table, the function returns an error message.
#' @export
#'
#' @examples
#' noncon_thres_value(20)
#'
#' noncon_thres_value(5000)
noncon_thres_value <- function(nominal_quantity) {
  thres_value <- max_errors %>%
    filter(.data$lower_limit <= nominal_quantity, .data$upper_limit >= nominal_quantity) %>%
    mutate(max_error = if_else(is.na(.data$percentage_over_nominal_quantity), .data$absolute_value,
                               nominal_quantity * (.data$percentage_over_nominal_quantity / 100))) %>%
    pull(.data$max_error) %>% as.numeric

  if(length(thres_value) > 0) {
    nominal_quantity - thres_value
  } else {
    stop("Nominal quantity is outside the range of the reference table.")
  }
}

#' Limit Acceptance Value
#'
#' Calculates the maximum acceptable value for a given nominal quantity, based on a
#' reference table of maximum errors.
#'
#' @param nominal_quantity A numeric value indicating the nominal quantity for which
#'   the maximum acceptable value is to be calculated.
#'
#' @return A numeric value indicating the maximum acceptable value for the given
#'   nominal quantity, which is twice the maximum error tolerance calculated by the
#'   "noncon_thres_value" function. If the nominal quantity is outside the range of the
#'   reference table, the function returns an error message.
#' @export
#'
#' @examples
#' noncon_limit_value(20)
#'
#' noncon_limit_value(5000)

noncon_limit_value <- function(nominal_quantity) {
  nominal_quantity - (noncon_thres_value(nominal_quantity) - nominal_quantity) * -2
}


#' Get Acceptance and Rejection Limits for Batch Size and Analysis Type
#'
#' Given a batch size and an analysis type (first or second), returns a list of
#' the acceptance and rejection limits for that batch size and analysis type.
#'
#' @param batch_size A numeric value indicating the size of the batch for which
#' the acceptance and rejection limits are to be returned.
#'
#' @param analysis A string indicating the analysis type for which the
#' acceptance and rejection limits are to be returned. Must be one of "first" or
#' "second".
#'
#' @return A list with two elements, named "analysis_acceptance" and "analysis_rejection",
#' representing the acceptance and rejection limits for the specified batch size
#' and analysis type.
#' If the batch size or analysis type is outside the range of valid values, an error
#' will be thrown with a message indicating the nature of the problem.
#'
#' @export
#'
#' @examples
#' get_limits(1500, "first")
#' get_limits(100, "second")
get_limits <- function(batch_size, analysis) {
  if (!analysis %in% c("first", "second")) {
    stop("Analysis parameter must be either 'first' or 'second'.")
  }

  limit_col <- paste0(analysis, "_analysis_acceptance")
  reject_col <- paste0(analysis, "_analysis_rejection")

  limits <- noncon_analysis_limits %>%
    filter(batch_size >= .data$batch_size_min, batch_size <= .data$batch_size_max) %>%
    select(analysis_acceptance = all_of(limit_col), analysis_rejection = all_of(reject_col)) %>%
    slice(1) %>%
    unlist() %>%
    as.list()

  if(length(limits) > 0) {
    limits
  } else {
    stop("Batch size is outside the range of possible values.")
  }
}

#' Perform First Non-Conformity Analysis on Sample Data
#'
#' Given a data frame of sample data, a column name, a nominal quantity, and a batch size,
#' calculates the number of non-conformities and determines whether the batch should be
#' accepted, rejected, or subjected to further analysis.
#'
#' @param df A data frame containing the sample data for analysis.
#' @param col_name A string representing the name of the column to analyze within the data frame.
#' @param nominal_quantity A numeric value indicating the nominal quantity of the product being produced.
#' @param batch_size A numeric value indicating the size of the batch being produced.
#'
#' @return A list with two elements: the decision on whether the batch should be accepted, rejected,
#' or subjected to further analysis, and the input data frame with a new column indicating whether each
#' value in the column being analyzed is acceptable, non-conforming, or unacceptable.
#'
#' @export
#'
#' @examples
#' df <- data.frame(value = c(rep(100, 25),c(99, 99, 99, 99, 91)))
#' result <- first_noncon_analysis(df, "value", 100, 100)
#' result$decision
#' result$df
first_noncon_analysis <- function(df, col_name, nominal_quantity, batch_size) {
  if (nrow(df) != get_sample_sizes(batch_size, "first")) {
    stop("The number of rows in the input dataframe does not match the required sample size.")
  }

  if (!col_name %in% names(df)) {
    stop("The col_name argument must be a column index value of the input dataframe.")
  }

  # Get acceptance and rejection limits for the batch size selected
  limits <- get_limits(batch_size, "first")
  acceptance_limit <- limits$analysis_acceptance
  rejection_limit <- limits$analysis_rejection

  # Get non-conformity threshold and limit value
  noncon_thres <- noncon_thres_value(nominal_quantity)
  noncon_limit <- noncon_limit_value(nominal_quantity)

  # Calculate non-conformities and rejection values
  df$status <- case_when(
    df[[col_name]] < noncon_limit ~ "Rejection",
    df[[col_name]] < noncon_thres ~ "Non-conformity",
    TRUE ~ "Aceptable"
  )

  # Count non-conformities
  num_noncon <- sum(df$status == "Non-conformity")

  # Decision process
  if (any(df$status == "Rejection")) {
    decision <- "Reject"
  } else if (num_noncon >= rejection_limit) {
    decision <- "Reject"
  } else if (num_noncon <= acceptance_limit) {
    decision <- "Accept"
  } else {
    decision <- "Second analysis"
  }

  return(list(decision = decision, df = df))
}

#' Perform Second Non-Conformity Analysis on Two Sample Data Sets
#'
#' Given two data frames of sample data, column names, a nominal quantity, and a batch size,
#' concatenates the two data frames, calculates the number of non-conformities, and determines
#' whether the batch should be accepted or rejected.
#'
#' @param first_analysis_df A data frame containing the sample data from the first analysis.
#' @param first_analysis_col_name A string representing the name of the column to analyze in the first analysis data frame.
#' @param second_analysis_df A data frame containing the sample data from the second analysis.
#' @param second_analysis_col_name A string representing the name of the column to analyze in the second analysis data frame.
#' @param nominal_quantity A numeric value indicating the nominal quantity of the product being produced.
#' @param batch_size A numeric value indicating the size of the batch being produced.
#'
#' @return A list with three elements: the decision on whether the batch should be accepted or rejected,
#' the first analysis input data frame with a new column indicating whether each value in the column being
#' analyzed is acceptable, non-conforming, or unacceptable, and the second analysis input data frame
#' with a new column indicating whether each value in the column being analyzed is acceptable, non-conforming,
#' or unacceptable.
#'
#' @export
#'
#' @examples
#' first_df <- data.frame(value1 = c(rep(100, 25),c(99, 99, 99, 99, 91)))
#' second_df <- data.frame(value2 = c(rep(100, 25),c(99, 99, 99, 99, 91)))
#' result <- first_noncon_analysis(first_df, "value1", second_df, "value2", 100, 100)
#' result$decision
#' result$df
second_noncon_analysis <- function(first_analysis_df, first_analysis_col_name, second_analysis_df, second_analysis_col_name, nominal_quantity, batch_size) {
  if ((nrow(second_analysis_df) != get_sample_sizes(batch_size, "second")) || (nrow(first_analysis_df) != get_sample_sizes(batch_size, "first"))) {
    stop("The number of rows in the input dataframe does not match the required sample size.")
  }

  if ((!first_analysis_col_name %in% names(first_analysis_df)) || (!second_analysis_col_name %in% names(second_analysis_df))) {
    stop("The col_name argument must be a column index value of the input dataframe.")
  }

  # Get acceptance and rejection limits for the batch size selected
  limits <- get_limits(batch_size, "second")
  acceptance_limit <- limits$analysis_acceptance
  rejection_limit <- limits$analysis_rejection

  # Get non-conformity threshold and limit value
  noncon_thres <- noncon_thres_value(nominal_quantity)
  noncon_limit <- noncon_limit_value(nominal_quantity)

  #Combine samples
  df <- bind_rows(
    first_analysis_df %>% mutate(sample = "First sample"),
    second_analysis_df %>% mutate(sample = "Second sample")
  ) %>%
    select(values = c(all_of(first_analysis_col_name), all_of(second_analysis_col_name)), sample)

  # Calculate non-conformities and rejection values
  df$status <- case_when(
    df[["values"]] < noncon_limit ~ "Rejection",
    df[["values"]] < noncon_thres ~ "Non-conformity",
    TRUE ~ "Aceptable"
  )

  first_analysis_df$status <- case_when(
    first_analysis_df[[first_analysis_col_name]] < noncon_limit ~ "Rejection",
    first_analysis_df[[first_analysis_col_name]] < noncon_thres ~ "Non-conformity",
    TRUE ~ "Aceptable"
  )

  second_analysis_df$status <- case_when(
    second_analysis_df[[second_analysis_col_name]] < noncon_limit ~ "Rejection",
    second_analysis_df[[second_analysis_col_name]] < noncon_thres ~ "Non-conformity",
    TRUE ~ "Aceptable"
  )

  # Count non-conformities
  num_noncon <- sum(df$status == "Non-conformity")

  # Decision process
  if (any(df$status == "Rejection")) {
    decision <- "Reject"
  } else if (num_noncon >= rejection_limit) {
    decision <- "Reject"
  } else {
    decision <- "Accept"
  }

  return(list(decision = decision, first_analysis_df = first_analysis_df, second_analysis_df = second_analysis_df))
}

#' Calculate the standard deviation estimate of a column in a data frame
#'
#' Given a data frame and the name of a column, calculates the estimate of the standard deviation
#' using the following formulas:
#' 1. Estimate of variance = (corrected sum of squares)/(n - 1)
#' 2. Corrected sum of squares = sum(x^2) - (sum(x)^2)/n
#' 3. Estimate of standard deviation = sqrt(estimate of variance)
#'
#' @param df A data frame containing the column to calculate the standard deviation estimate.
#' @param col_name A string representing the name of the column for which to calculate the estimate.
#'
#' @return A numeric value representing the estimate of the standard deviation of the column.
#'
#' @export
#'
#' @examples
#' data(mtcars)
#' std_dev_estimate <- std_dev_estimate(mtcars, "mpg")
#' std_dev_estimate
std_dev_estimate <- function(df, col_name) {

  # Calculate corrected sum of squares
  x <- df[[col_name]]
  n <- length(x)
  sum_x <- sum(x)
  sum_x_squared <- sum(x^2)
  corrected_sum_squares <- sum_x_squared - (sum_x^2)/n

  # Calculate estimate of variance
  estimate_variance <- corrected_sum_squares/(n - 1)

  # Calculate estimate of standard deviation
  estimate_std_dev <- sqrt(estimate_variance)

  return(estimate_std_dev)
}

#' Perform Mean Analysis on Sample Data
#'
#' Given a data frame of sample data, a column name, a nominal quantity, and a batch size,
#' calculates the mean of the column and determines whether the batch should be
#' accepted or rejected based on the comparison of the mean with the nominal quantity minus
#' the estimation of the standard deviation.
#'
#' @param df A data frame containing the sample data for analysis.
#' @param col_name A string representing the name of the column to analyze within the data frame.
#' @param batch_size A numeric value indicating the size of the batch being produced.
#' @param nominal_quantity A numeric value indicating the nominal quantity of the product being produced.
#'
#' @return A string indicating whether the batch should be accepted or rejected based on the mean analysis.
#'
#' @examples
#' df <- data.frame(values = rep(10,30))
#' result <- mean_analysis(df, "values", 500, 10)
#' result
#'
#' @export
mean_analysis <- function(df, col_name, batch_size, nominal_quantity) {
  if (nrow(df) != get_sample_sizes(batch_size, "mean")) {
    stop("The number of rows in the input dataframe does not match the required sample size.")
  }

  if (!col_name %in% names(df)) {
    stop("The col_name argument must be a column index value of the input dataframe.")
  }

  std_dev <- std_dev_estimate(df, col_name)
  mean_val <- mean(df[[col_name]])
  computed_student_dist <- mean_analysis_params %>%
    filter(batch_size >= .data$batch_size_min, batch_size <= .data$batch_size_max) %>%
    pull(.data$computed_student_distribution)

  # Calculate the limit
  upper_limit <- nominal_quantity - std_dev * computed_student_dist

  if(mean_val >= upper_limit) {
    return("Accept")
  } else {
    return("Reject")
  }
}
