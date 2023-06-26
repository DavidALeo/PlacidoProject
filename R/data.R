#' Sample sizes corresponding to each batch size.
#'
#' A dataset containing the sample sizes corresponding to each batch size
#'  using the specification on RD 1802/20XX
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{batch_size_min}{lower limit of the batch size interval}
#'   \item{batch_size_max}{upper limit of the batch size interval}
#'   \item{first_analysis_sample }{sample needed for the first non-conformities check}
#'   \item{second_analysis_sample }{sample needed for the second non-conformities check}
#'   \item{mean_analysis_sample }{sample needed for the mean analysis}
#' }
"sample_sizes"

#' Maximum error tolerances for nominal quantities.
#'
#' A dataset containing the maximum error tolerances for different nominal
#' quantities, according to the specifications provided in the table.
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#' \item{lower_limit}{lower limit of the nominal quantity range}
#' \item{upper_limit}{upper limit of the nominal quantity range}
#' \item{percentage_over_nominal_quantity}{percentage of error over nominal quantity (if available)}
#' \item{absolute_value}{absolute value of the maximum error tolerance (if available)}
#' }
"max_errors"

#' Acceptance and Rejection limits by batch size
#'
#' A dataset containing the acceptance and rejection limits for batches of different sizes
#' according to the specifications of RD 1802/20XX.
#'
#' @format A data frame with 3 rows and 6 variables:
#' \describe{
#' \item{batch_size_min}{The lower limit of the batch size interval.}
#' \item{batch_size_max}{The upper limit of the batch size interval.}
#' \item{first_analysis_acceptance}{The number of pieces that pass the first analysis and are accepted.}
#' \item{first_analysis_rejection}{The number of pieces that fail the first analysis and are rejected.}
#' \item{second_analysis_acceptance}{The number of pieces that pass the second analysis and are accepted.}
#' \item{second_analysis_rejection}{The number of pieces that fail the second analysis and are rejected.}
#' }
"noncon_analysis_limits"

#' Mean analysis parameters
#'
#' A dataset containing the required sample sizes for mean analysis and the corresponding computed Student distribution
#' using the specification on RD 1802/20XX.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#' \item{batch_size_min}{lower limit of the batch size interval}
#' \item{batch_size_max}{upper limit of the batch size interval}
#' \item{sample_sizes}{sample sizes required for the mean analysis for each batch size}
#' \item{computed_student_distribution}{Student distribution computed for the corresponding sample sizes}
#' }
"mean_analysis_params"

#' Sample dataset
#'
#' A dataset containing an example to run the package.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#' \item{Id}{Id of the package}
#' \item{Paquete}{Package}
#' \item{Volumen}{Effective quantity of the package}
#' \item{Fecha}{Date of the measurement}
#' }
"sample_dataset"

