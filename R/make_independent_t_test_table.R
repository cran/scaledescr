#' Create a one-row summary table for an independent-samples test
#'
#' This function performs an independent-samples test (Welch's t-test, Student's t-test,
#' or Mann-Whitney U test) between two groups defined by a binary grouping variable
#' and returns a single-row data frame. The output includes group names,
#' sample sizes, mean difference, test statistics, p-value, and effect size
#' (Cohen's d) with a qualitative interpretation.
#'
#' The function is intended for streamlined reporting and does not introduce
#' new statistical methods. Computations rely on \code{stats::t.test()} or \code{stats::wilcox.test()}.
#'
#' @param data A data frame containing the outcome and grouping variables.
#' @param outcome Character string specifying the numeric outcome variable.
#' @param group Character string specifying the grouping variable. Must have exactly two levels.
#' @param test_type Character string specifying the test method: \code{"welch"} (default),
#'   \code{"student"}, or \code{"mann-whitney"}.
#'
#' @return A single-row data frame with the following columns:
#' \itemize{
#'   \item \code{test}: Name of the statistical test
#'   \item \code{group1}, \code{group2}: Group labels
#'   \item \code{mean_diff}: Mean difference between groups (group1 - group2)
#'   \item \code{t_value}: Test statistic (t-value or W-statistic for Mann-Whitney)
#'   \item \code{df}: Degrees of freedom (returns \code{NA} for Mann-Whitney)
#'   \item \code{p_value}: p-value
#'   \item \code{n_group1}, \code{n_group2}: Sample sizes per group
#'   \item \code{cohens_d}: Cohen's d effect size
#'   \item \code{interpretation}: Qualitative interpretation of effect size
#' }
#'
#' @details
#' Welch’s t-test is used by default, which does not assume equal variances.
#' Cohen’s d is computed using the pooled standard deviation for comparability
#' with conventional benchmarks. Group ordering follows the factor level order
#' of the grouping variable.
#'
#' @examples
#' set.seed(123)
#'
#' data_t <- data.frame(
#'   group = rep(c("CBT", "Psychodynamic"), each = 30),
#'   score = c(
#'     rnorm(30, mean = 18, sd = 4),
#'     rnorm(30, mean = 21, sd = 4)
#'   )
#' )
#'
#' # Legacy call (defaults to welch)
#' make_independent_t_test_table(data = data_t, outcome = "score", group = "group")
#'
#' # New explicitly defined calls
#' make_independent_t_test_table(data_t, "score", "group", test_type = "student")
#' make_independent_t_test_table(data_t, "score", "group", test_type = "mann-whitney")
#'
#' @importFrom stats sd t.test wilcox.test
#' @export
make_independent_t_test_table <- function(data, outcome, group, test_type = c("welch", "student", "mann-whitney")) {

  # Ensure backward compatibility if argument is omitted
  test_type <- match.arg(test_type)

  # Extract variables
  y <- data[[outcome]]
  g <- data[[group]]

  if (length(unique(na.omit(g))) != 2) {
    stop("Grouping variable must have exactly two levels")
  }

  # Split and calculate foundational group parameters
  grp <- split(y, g)
  m1 <- mean(grp[[1]], na.rm = TRUE)
  m2 <- mean(grp[[2]], na.rm = TRUE)
  s1 <- sd(grp[[1]], na.rm = TRUE)
  s2 <- sd(grp[[2]], na.rm = TRUE)
  n1 <- length(na.omit(grp[[1]]))
  n2 <- length(na.omit(grp[[2]]))

  # Conditional Engine for execution based on test_type choice
  if (test_type == "welch") {
    label_text <- "Independent Welch t-test"
    t_obj      <- stats::t.test(y ~ g, var.equal = FALSE)
    stat_val   <- round(unname(t_obj$statistic), 3)
    df_val     <- round(unname(t_obj$parameter), 2)
    p_val      <- t_obj$p.value

  } else if (test_type == "student") {
    label_text <- "Independent Student t-test"
    t_obj      <- stats::t.test(y ~ g, var.equal = TRUE)
    stat_val   <- round(unname(t_obj$statistic), 3)
    df_val     <- round(unname(t_obj$parameter), 2)
    p_val      <- t_obj$p.value

  } else if (test_type == "mann-whitney") {
    label_text <- "Mann-Whitney U test"
    w_obj      <- stats::wilcox.test(y ~ g, exact = FALSE)
    stat_val   <- round(unname(w_obj$statistic), 3) # This outputs the W statistic
    df_val     <- NA                                # Non-parametric tests have no t-df
    p_val      <- w_obj$p.value
  }

  # Pooled SD calculation (kept for standardized benchmark scaling metrics)
  sd_pooled <- sqrt(
    ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) /
      (n1 + n2 - 2)
  )

  # Cohen's d metric
  d <- (m1 - m2) / sd_pooled

  interpretation <- cut(
    abs(d),
    breaks = c(-Inf, 0.2, 0.5, 0.8, Inf),
    labels = c("Negligible", "Small", "Medium", "Large")
  )

  # Construct and output final reporting row
  data.frame(
    test           = label_text,
    group1         = names(grp)[1],
    group2         = names(grp)[2],
    mean_diff      = round(m1 - m2, 3),
    t_value        = stat_val,
    df             = df_val,
    p_value        = p_val,
    n_group1       = n1,
    n_group2       = n2,
    cohens_d       = round(d, 3),
    interpretation = as.character(interpretation),
    stringsAsFactors = FALSE
  )
}
