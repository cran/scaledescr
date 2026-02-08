#' Create a Descriptive Statistics Table Row
#'
#' Formats a descriptive statistics object into a single-row data frame
#'
#' This function is intended for reporting descriptive statistics of
#' total scores that has been calculated using psych::describe().
#'
#' @param descr_object An object returned by \code{psych::describe()}.
#'   Must contain: \code{n}, \code{mean}, \code{sd}, \code{median},
#'   \code{min}, \code{max}, \code{skew}, \code{kurtosis}.
#' @param scale_name A single character string specifying the name of the scale.
#'
#' @return A single-row data frame with formatted descriptive statistics.
#' @examples
#' {
#'   # Create 10 random PHQ-9 scores
#'   phq9_data <- as.data.frame(matrix(sample(0:3, 10 * 9, replace = TRUE), 10, 9))
#'   colnames(phq9_data) <- paste0("Q", 1:9)
#'   phq9_data$total <- rowSums(phq9_data)
#'   descr_total <- psych::describe(phq9_data$total)
#'   make_scale_description_table(descr_total, scale_name = "PHQ-9")
#' }

#' @export
#'
make_scale_description_table <- function(descr_object, scale_name) {
  ## ---- Gentle checks ----

  if (!is.list(descr_object)) {
    stop("descr_object must be an object returned by psych::describe().")
  }

  if (!is.character(scale_name) || length(scale_name) != 1) {
    stop("scale_name must be a single character string (e.g., 'PHQ 9').")
  }

  required_fields <- c(
    "n", "mean", "sd", "median", "min", "max", "skew", "kurtosis"
  )

  missing_fields <- setdiff(required_fields, names(descr_object))

  if (length(missing_fields) > 0) {
    stop(
      "descr_object is missing required fields: ",
      paste(missing_fields, collapse = ", "),
      "\nDid you pass the output of psych::describe()?"
    )
  }

  ## ---- Create table Row----

  data.frame(
    Scale    = scale_name,
    N        = descr_object$n,
    Mean     = round(descr_object$mean, 2),
    SD       = round(descr_object$sd, 2),
    Median   = round(descr_object$median, 2),
    Range    = paste0(round(descr_object$min, 2), "-", (round(descr_object$max, 2))),
    Skewness = round(descr_object$skew, 2),
    Kurtosis = round(descr_object$kurtosis, 2)
  )
}
