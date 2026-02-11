#' Create a one-row summary table for a chi-square test
#'
#' This function formats the result of a pre-computed \code{chisq.test()}
#' into a single-row, report-ready data frame. It supports goodness-of-fit,
#' independence, and homogeneity chi-square tests and includes an appropriate
#' effect size with a qualitative interpretation.
#'
#' The function does not perform the chi-square test itself and does not
#' introduce new statistical methods. All test statistics are extracted
#' directly from the supplied \code{chisq.test()} object.
#'
#' @param chisq_object An object of class \code{"htest"} produced by
#'   \code{stats::chisq.test()}.
#' @param test_type Character string specifying the type of chi-square test.
#'   One of \code{"goodness-of-fit"}, \code{"independence"}, or
#'   \code{"homogeneity"}.
#'
#' @return A single-row data frame with the following columns:
#' \itemize{
#'   \item \code{test}: Type of chi-square test
#'   \item \code{chi_square}: Chi-square statistic
#'   \item \code{df}: Degrees of freedom
#'   \item \code{p_value}: p-value
#'   \item \code{N}: Total sample size
#'   \item \code{effect_size}: Effect size (Cohen’s w or Cramér’s V)
#'   \item \code{effect_type}: Type of effect size reported
#'   \item \code{effect_interpretation}: Qualitative interpretation of effect size
#' }
#'
#' @details
#' For goodness-of-fit tests, Cohen’s w is reported. For tests of independence
#' and homogeneity, Cramér’s V is reported. Effect size interpretations follow
#' conventional benchmarks (0.10 = small, 0.30 = medium, 0.50 = large).
#'
#' @examples
#' # Goodness-of-fit example
#' observed <- c(40, 30, 50)
#' chisq_gof <- chisq.test(observed)
#'
#' make_chisq_test_table(
#'   chisq_object = chisq_gof,
#'   test_type = "goodness-of-fit"
#' )
#'
#' # Independence test example
#' tbl <- matrix(c(20, 30, 10, 40), nrow = 2)
#' chisq_ind <- chisq.test(tbl)
#'
#' make_chisq_test_table(
#'   chisq_object = chisq_ind,
#'   test_type = "independence"
#' )
#'
#' @export
make_chisq_test_table <- function(
  chisq_object,
  test_type = c("goodness-of-fit", "independence", "homogeneity")
) {
  test_type <- match.arg(test_type)

  if (!inherits(chisq_object, "htest")) {
    stop("Input must be a chisq.test() result")
  }

  chi_sq <- unname(chisq_object$statistic)
  df <- unname(chisq_object$parameter)
  p_value <- chisq_object$p.value
  N <- sum(chisq_object$observed)

  if (test_type == "goodness-of-fit") {
    effect_type <- "Cohen_w"
    effect_size <- sqrt(chi_sq / N)
    cutoffs <- c(0.10, 0.30, 0.50)
  } else {
    effect_type <- "Cramers_V"
    k <- min(dim(chisq_object$observed)) - 1
    effect_size <- sqrt(chi_sq / (N * k))
    cutoffs <- c(0.10, 0.30, 0.50)
  }

  effect_label <- ifelse(
    effect_size < cutoffs[1], "negligible",
    ifelse(effect_size < cutoffs[2], "small",
      ifelse(effect_size < cutoffs[3], "medium", "large")
    )
  )

  data.frame(
    test = test_type,
    chi_square = chi_sq,
    df = df,
    p_value = p_value,
    N = N,
    effect_size = effect_size,
    effect_type = effect_type,
    effect_interpretation = effect_label,
    stringsAsFactors = FALSE
  )
}
