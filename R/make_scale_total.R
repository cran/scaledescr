#' Compute Scale Total Score
#'
#' @description
#' Computes the total score for a psychometric scale by summing specified
#' numeric variables. The resulting total score is appended as a new column
#' to the input data frame. Missing values are handled using `na.rm = TRUE`.
#'
#' @param data A data frame.
#' @param vars A character vector of column names to be summed.
#' @param new_var A single character string specifying the name of the
#'   new total score column.
#'
#' @return The input data frame with one additional numeric column.
#' @examples
#' # Example using the standard 'bfi' (Big Five Inventory) dataset from psych
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   data(bfi, package = "psych")
#'
#'   # Take a small subset for demonstration
#'   sample_bfi <- head(bfi, 10)
#'
#'   # Compute the Agreeableness total score (items A1 to A5)
#'   sample_bfi <- make_scale_total(
#'     data = sample_bfi,
#'     vars = paste0("A", 1:5),
#'     new_var = "Agreeableness_Total"
#'   )
#'
#'   # Compute the Extraversion total score (items E1 to E5)
#'   sample_bfi <- make_scale_total(
#'     data = sample_bfi,
#'     vars = paste0("E", 1:5),
#'     new_var = "Extraversion_Total"
#'   )
#'
#'   # View the newly appended score columns alongside the items
#'   sample_bfi[, c("A1", "Agreeableness_Total", "E1", "Extraversion_Total")]
#' }
#' @export
make_scale_total <- function(data, vars, new_var) {

  # Validate data
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  # Validate vars
  if (!is.character(vars) || length(vars) == 0) {
    stop("`vars` must be a non-empty character vector.", call. = FALSE)
  }

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      paste("Variables not found in data:",
            paste(missing_vars, collapse = ", ")),
      call. = FALSE
    )
  }

  # Validate new_name
  if (!is.character(new_var) || length(new_var) != 1) {
    stop("`new_var` must be a single character string.", call. = FALSE)
  }

  if (new_var %in% names(data)) {
    stop(
      paste("Column", new_var, "already exists in data."),
      call. = FALSE
    )
  }

  # Subset and check numeric
  sub_data <- data[, vars, drop = FALSE]

  non_numeric <- names(sub_data)[!vapply(sub_data, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      paste("Non-numeric variables detected:",
            paste(non_numeric, collapse = ", ")),
      call. = FALSE
    )
  }

  # Compute total
  data[[new_var]] <- rowSums(sub_data, na.rm = TRUE)

  data
}
