#' Replace Missing Values with Medians
#'
#' This function replaces all missing values in numeric columns with the median of their respective columns.
#'
#' @param data A data frame with numeric columns containing missing values.
#' @return A data frame with missing values replaced by column medians.
#' @examples
#' data(airquality)
#' cleaned_airquality <- replace_missing_with_median(airquality)
#' print(cleaned_airquality)
#' @export
replace_missing_with_median <- function(data) {
  medians <- calculate_median(data)

  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]][is.na(data[[col]])] <- medians[col]
    }
  }

  return(data)
}
