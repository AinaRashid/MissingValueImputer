#' Calculate Median Values
#'
#' This function calculates the median for each numeric column in a dataset. Non-numeric columns are ignored.
#'
#' @param data A data frame containing the variables to calculate medians for.
#' @return A named numeric vector of median values for each numeric column. Non-numeric columns return `NA`.
#' @examples
#' data(airquality)
#' calculate_median(airquality)
#' @export
calculate_median <- function(data) {
  sapply(data, function(x) {
    if (is.numeric(x)) median(x, na.rm = TRUE) else NA
  })
}
