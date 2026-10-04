#' Remove Columns from Data Frame
#'
#' @description
#' Removes one or more specified columns from a data frame by name.
#' The function accepts either a single column name or a character vector
#' of multiple column names. Missing columns are reported without crashing
#' the script.
#'
#' @param data A data frame.
#' @param column A character vector of column names to be removed.
#'
#' @return The input data frame with the specified columns removed.
#' @examples
#' # Example using the standard 'bfi' (Big Five Inventory) data set from psych
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   data(bfi, package = "psych")
#'
#'   # Take a small subset for demonstration
#'   sample_bfi <- head(bfi, 10)
#'
#'   # Remove a single column by name
#'   sample_bfi <- scaledescr_remove_column(
#'     data = sample_bfi,
#'     column = "education"
#'   )
#'
#'   # Remove multiple columns using paste0()
#'   sample_bfi <- scaledescr_remove_column(
#'     data = sample_bfi,
#'     column = paste0("E", 1:5)
#'   )
#' }
#' @export
scaledescr_remove_column <- function(data, column) {

  # Validate data input
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  # Validate column input
  if (!is.character(column) || length(column) == 0) {
    stop("`column` must be a non-empty character vector.", call. = FALSE)
  }

  # Check for columns that do not exist in the dataset
  missing_cols <- setdiff(column, names(data))
  if (length(missing_cols) > 0) {
    warning(
      paste("The following column(s) were not found in the data and will be skipped:",
            paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Filter out the specified columns that do exist in the dataset
  cols_to_keep <- !(names(data) %in% column)
  updated_data <- data[, cols_to_keep, drop = FALSE]

  updated_data
}
