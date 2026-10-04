#' Merge Counterbalanced Item Pairs of Scale Administration Columns
#'
#' @description
#' Merges mutually exclusive item pairs resulting from counterbalanced survey
#' administrations,generated during counterbalanced psychometric testing (e.g., online survey exports from PsyToolkit, Google Forms, or Qualtrics),and also handling blank strings, checking for missing columns, and
#' flagging responses containing data in both versions.
#'
#' **Purpose:** In counterbalanced designs (e.g., English vs. Gujarati version, or
#' Order 1 vs. Order 2), each participant answers only one branch, so the export
#' contains two sparse columns per item, each empty (`NA` or `""`) for half the
#' sample.Such data is generated when same scales area administered in counterbalanced order using platforms like PsyToolkit
#'
#' This function coalesces paired administration vectors into a unified set of item columns
#' (e.g., merging `phq_e_1_1` and `phq_e_2_1` into `phq_e_1`), checks for data-entry or branching
#' overlap errors, and prepares the dataset for downstream psychometric evaluation
#'
#' @param data A data frame.
#' @param col_1 Character vector of column names for version 1.
#' @param col_2 Character vector of column names for version 2.
#' @param new_columns Character vector of names for the merged output columns.
#' @param on_conflict Character string specifying action if a participant answered both columns:
#'   \code{"warning"} (default; logs warning, keeps \code{col_1}), \code{"error"} (stops execution),
#'   or \code{"silent"} (silently keeps \code{col_1}).
#'
#' @return A data frame with merged columns appended.
#' @examples
#' \dontrun{
#' # PHQ-9 administered in English and Gujarati, each across two orders
#'
#' # Merge English items across orders -> phq_e_1 ... phq_e_9
#' df <- merge_counterbalanced_columns(
#'   data        = df,
#'   col_1       = paste0("phq_e_1_", 1:9),
#'   col_2       = paste0("phq_e_2_", 1:9),
#'   new_columns = paste0("phq_e_", 1:9),
#'   on_conflict = "warning"
#' )
#'
#' # Merge Gujarati items across orders -> phq_g_1 ... phq_g_9
#' df <- merge_counterbalanced_columns(
#'   data        = df,
#'   col_1       = paste0("phq_g_1_", 1:9),
#'   col_2       = paste0("phq_g_2_", 1:9),
#'   new_columns = paste0("phq_g_", 1:9)
#' )
#'
#' # Inspect the merged items
#' head(df[c("age", "gender", paste0("phq_g_", 1:9))])
#' }
#' @export
merge_counterbalanced_columns <- function(data,
                                 col_1,
                                 col_2,
                                 new_columns,
                                 on_conflict = c("warning", "error", "silent")) {
  on_conflict <- match.arg(on_conflict)

  # Input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (length(col_1) != length(col_2) || length(col_1) != length(new_columns)) {
    stop("`col_1`, `col_2`, and `new_columns` must have identical lengths.", call. = FALSE)
  }
  missing_cols <- setdiff(c(col_1, col_2), names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in `data`:", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  conflicts_detected <- character()

  for (i in seq_along(col_1)) {
    c1 <- col_1[i]
    c2 <- col_2[i]
    v1 <- data[[c1]]
    v2 <- data[[c2]]

    # Treat empty strings and pure whitespace as NA
    if (is.character(v1)) v1[trimws(v1) == ""] <- NA
    if (is.character(v2)) v2[trimws(v2) == ""] <- NA

    # Check for conflicts (both columns have valid values)
    overlap_idx <- which(!is.na(v1) & !is.na(v2))
    if (length(overlap_idx) > 0) {
      conflict_msg <- sprintf("'%s' and '%s' have overlapping data in row(s): %s",
                              c1, c2, paste(overlap_idx, collapse = ", "))
      conflicts_detected <- c(conflicts_detected, conflict_msg)
    }

    # Merge: take v1 if present, otherwise v2
    data[[new_columns[i]]] <- ifelse(!is.na(v1), v1, v2)
  }

  # Handle conflicts based on user setting
  if (length(conflicts_detected) > 0) {
    full_msg <- paste(conflicts_detected, collapse = "\n  ")
    if (on_conflict == "error") {
      stop(paste0("Counterbalance conflicts detected:\n  ", full_msg), call. = FALSE)
    } else if (on_conflict == "warning") {
      warning(paste0("Counterbalance conflicts detected (defaulted to `col_1`):\n  ", full_msg), call. = FALSE)
    }
  }

  data
}
