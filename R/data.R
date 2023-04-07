#' Sample sizes corresponding to each batch size.
#'
#' A dataset containing the sample sizes corresponding to each batch size
#'  using the specification on RD 1802/20XX
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{batch_size_min}{lower limit of the batch size interval}
#'   \item{batch_size_max}{upper limit of the batch size interval}
#'   \item{sample1_size}{sample needed for the first non-conformitis check}
#'   \item{sample2_size}{sample needed for the second non-conformitis check}
#' }
"sizes_df"
