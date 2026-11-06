#' Compute Intraclass Correlation Coefficient (ICC) for Selected Columns
#'
#' @description
#' Computes Intraclass Correlation Coefficients (ICC) for a specified set
#' of numeric variables within a data frame using \code{psych::ICC()}.
#'
#' This function standardizes column selection and basic validation while
#' delegating all statistical estimation to \code{psych::ICC()}.
#'
#' @param data A data frame containing the ratings or measurements.
#' @param items A character vector specifying column names for which
#'   ICC should be calculated (e.g., raters or repeated measurements).
#' @param check_numeric Logical. If \code{TRUE} (default), ensures all
#'   selected columns are numeric.
#' @param lmer Logical. Passed to \code{psych::ICC()}. Default is FALSE.
#' @param ... Additional arguments passed to \code{psych::ICC()}.
#'
#' @return An object returned by \code{psych::ICC()}, typically a list
#'   containing a results table with ICC estimates, F statistics,
#'   confidence intervals, and model details.
#'
#' @details
#' The function does not choose or filter specific ICC models.
#' All available ICC types (e.g., ICC1, ICC2, ICC3, single and average)
#' are returned exactly as produced by \code{psych::ICC()}.
#'
#' Users are responsible for:
#' \itemize{
#'   \item Choosing the appropriate ICC model for their study design.
#'   \item Ensuring assumptions (e.g., continuous ratings, independence).
#'   \item Handling missing data appropriately.
#' }
#' @seealso
#' \code{\link[psych]{ICC}}
#' @note
#' For test-retest, cross-language, or rater-agreement designs across multi-item
#' scales, do \strong{not} pass all items simultaneously (e.g., all English and
#' Gujarati items together). Doing so treats every individual item as an
#' interchangeable rater/judge, pooling item-level variance with rater variance.
#'
#' Instead:
#' \itemize{
#'   \item For \strong{item-level equivalence}: compute ICC separately for each
#'         matched pair (2 judges per call; e.g., \code{phq_e_1} vs. \code{phq_g_1}).
#'   \item For \strong{scale-level equivalence}: compute the composite or total scores
#'         first, then pass the two total score columns (2 judges; e.g., \code{phq_total_e}
#'         vs. \code{phq_total_g}).
#' }
#' @importFrom psych ICC
#' @section Warning:
#' \strong{Do not pass all items of a multi-item scale simultaneously for test-retest or cross-language equivalence.}
#' Passing 18 columns (e.g., 9 English and 9 Gujarati items) treats every item as an interchangeable rater (\code{Judges = 18}).
#' This confounds item-specific variance with language differences, yielding misleading results.
#' For pairwise equivalence, evaluate items individually (\code{Judges = 2}) or sum/average them into composite scores first.
#'
#' @examples
#' data(counterbalance)
#'
#' # Step 1: Prepare merged columns across counterbalanced orders
#' df <- merge_counterbalanced_columns(
#'   data        = counterbalance,
#'   col_1       = paste0("phq_e_1_", 1:9),
#'   col_2       = paste0("phq_e_2_", 1:9),
#'   new_columns = paste0("phq_e_", 1:9)
#' )
#'
#' df <- merge_counterbalanced_columns(
#'   data        = df,
#'   col_1       = paste0("phq_g_1_", 1:9),
#'   col_2       = paste0("phq_g_2_", 1:9),
#'   new_columns = paste0("phq_g_", 1:9)
#' )
#'
#' # -------------------------------------------------------------
#' # CORRECT USE 1: Item-level cross-language equivalence (Judges = 2)
#' # -------------------------------------------------------------
#' compute_ICC(df, items = c("phq_e_1", "phq_g_1"))
#'
#' # -------------------------------------------------------------
#' # CORRECT USE 2: Scale-level cross-language equivalence (Judges = 2)
#' # -------------------------------------------------------------
#' df$phq_total_e <- rowSums(df[, paste0("phq_e_", 1:9)])
#' df$phq_total_g <- rowSums(df[, paste0("phq_g_", 1:9)])
#' compute_ICC(df, items = c("phq_total_e", "phq_total_g"))
#'
#' # -------------------------------------------------------------
#' # INCORRECT USE: What NOT to do for cross-language equivalence using `compute_ICC()` function!
#' # -------------------------------------------------------------
#' # Passing all 18 columns simultaneously treats each question as an
#' # interchangeable judge (Judges = 18), conflating internal consistency
#' # with cross-language agreement and producing misleading ICC estimates:
#' \dontrun{
#' compute_ICC(df, items = c(paste0("phq_e_", 1:9), paste0("phq_g_", 1:9)))
#' }
#' @export
compute_ICC <- function(data,
                        items,
                        check_numeric = TRUE,
                        lmer = FALSE,
                        ...) {

  # ---- Validate data ----
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (missing(items) || length(items) < 2) {
    stop("`items` must contain at least two column names.")
  }

  if (!all(items %in% names(data))) {
    missing_items <- items[!items %in% names(data)]
    stop(
      paste("The following items were not found in `data`:",
            paste(missing_items, collapse = ", "))
    )
  }

  # ---- Subset data ----
  sub_data <- data[, items, drop = FALSE]

  # ---- Numeric check ----
  if (check_numeric) {
    non_numeric <- names(sub_data)[!sapply(sub_data, is.numeric)]
    if (length(non_numeric) > 0) {
      stop(
        paste("The following items are not numeric:",
              paste(non_numeric, collapse = ", "))
      )
    }
  }

  # ---- Compute ICC using psych ----
  psych::ICC(sub_data, lmer = lmer, ...)
}
