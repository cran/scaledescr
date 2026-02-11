#' Create a Descriptive Statistics Table Row
#'
#' Computes and formats descriptive statistics for a scale total score into a single-row data frame suitable for reporting.
#'
#' This function is intended for reporting descriptive statistics of
#' total scale scores, for which descriptive statistics are computed internally
#' using \code{psych::describe()} or \code{base::summary()}.
#'
#' @param x A numeric vector representing total scores of a scale.
#' @param type Optional character string. If NULL (default), descriptive
#'   statistics are computed using \code{psych::describe()}. If set to
#'   \code{"summary"}, statistics are computed using \code{base::summary()}.

#' @param scale_name A single character string specifying the name of the scale.
#'
#' @return A single-row data frame with formatted descriptive statistics.
#'
#' @examples
#' {
#'   phq9_data <- as.data.frame(matrix(sample(0:3, 10 * 9, replace = TRUE), 10, 9))
#'   colnames(phq9_data) <- paste0("Q", 1:9)
#'   phq9_data$total <- rowSums(phq9_data)
#'
#'   make_scale_description_table(phq9_data$total, scale_name = "PHQ-9")
#' }
#'
#' @export
make_scale_description_table <- function(x, scale_name, type = NULL) {
  ## ---- Gentle checks ----
  if (!is.numeric(x)) {
    stop("x must be a numeric vector (e.g., total scale scores).", call. = FALSE)
  }

  if (!is.character(scale_name) || length(scale_name) != 1) {
    stop("scale_name must be a single character string.", call. = FALSE)
  }

  ## ---- psych::describe() branch ----
  if (is.null(type)) {
    descr <- psych::describe(x)

    out <- data.frame(
      Scale = scale_name,
      N = descr$n,
      Mean = round(descr$mean, 2),
      SD = round(descr$sd, 2),
      Median = round(descr$median, 2),
      Range = paste0(
        round(descr$min, 2), "-",
        round(descr$max, 2)
      ),
      Skewness = round(descr$skew, 2),
      Kurtosis = round(descr$kurtosis, 2)
    )

    ## ---- base::summary() branch ----
  } else if (type == "summary") {
    s <- summary(x)

    out <- data.frame(
      Scale = scale_name,
      N = sum(!is.na(x)),
      `1st Qu.` = round(s[["1st Qu."]], 2),
      Median = round(s[["Median"]], 2),
      Mean = round(s[["Mean"]], 2),
      `3rd Qu.` = round(s[["3rd Qu."]], 2),
      Range = paste0(
        round(s[["Min."]], 2), "-",
        round(s[["Max."]], 2)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    stop("type must be NULL or 'summary'.", call. = FALSE)
  }

  return(out)
}
