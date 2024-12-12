#' Detect Missing Values
#'
#' This function calculates the number of missing values (NA) in each column of a dataset.
#'
#' @param data A data frame to analyze for missing values.
#' @return A named numeric vector where each name is a column with missing values, and the value is the count of missing values in that column.
#' @examples
#' data(airquality)
#' detect_missing_values(airquality)
#' @export
detect_missing_values <- function(data) {
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)

  for (i in 1:ncol(data)){
    missing_counts[i] <- sum(is.na(data[[i]]))
  }

  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}
