#' Generate a Tidy Item-Total Correlation and Scale Reliability Table
#'
#' @description
#' `make_item_total_correlation_table` extracts a specified subset of items
#' from a dataset, executes an item-level psychometric reliability analysis
#' using the \(\code{performance}\) package, and returns a clean, publication-ready
#' data frame. It also prints overall scale reliability metrics to the console.
#'
#' @param data A data frame containing the questionnaire items or scale vectors to analyze.
#' @param columns A character vector specifying the exact column names of the items
#'   to include in the scale analysis (e.g.\(, \code{paste0("phq_e_", 1:9)}\)).
#'
#' @details
#' The function wraps \(\code{performance::item_reliability()}\) to isolate relevant metrics
#' for psychometric scale validation (such as translating or adapting existing instruments).
#' It strips away complex package metadata to output a standard, easily exportable
#' data frame.
#'
#' The returned data frame contains:
#' \itemize{
#'   \item \(\code{Item}:\) The name of each questionnaire item.
#'   \item \(\code{Alpha_If_Deleted}:\) The scale's overall Cronbach's Alpha if that specific item is removed.
#'   \item \(\code{Total_Correlation}:\) The uncorrected item-total correlation (includes the item itself).
#'   \item \(\code{Discrimination}:\) The corrected item-total correlation (excludes the target item).
#'     Values > 0.30 indicate good item cohesion.
#' }
#'
#' @return A standard data frame containing item-level statistics (Item, Alpha_If_Deleted,
#'   Total_Correlation, and Discrimination).
#'
#' @importFrom stats cor
#' @importFrom performance item_reliability
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required package data
#' data("counterbalance", package = "scaledescr")
#'
#' # Example A: Passing a pre-defined variable vector
#' phq_cols <- paste0("phq_e_", 1:9)
#' make_item_total_correlation_table(data = counterbalance, columns = phq_cols)
#'
#' # Example B: Directly inserting the column generation pattern
#' make_item_total_correlation_table(data = counterbalance, columns = paste0("phq_e_", 1:9))
#' }
make_item_total_correlation_table <- function(data, columns) {

  # Ensure the performance package is locally namespace-available
  if (!requireNamespace("performance", quietly = TRUE)) {
    stop("The 'performance' package must be installed to run this function.")
  }

  # 1. Subset data down to only the targeted questionnaire vectors
  items_subset <- data[, columns, drop = FALSE]

  # 2. Coerce columns to numeric to prevent underlying algebra crashes
  items_subset[] <- lapply(items_subset, function(x) as.numeric(as.character(x)))

  # 3. Execute item reliability engine in the background
  analysis_obj <- performance::item_reliability(items_subset)

  # 4. Coerce proprietary easystats printing matrix to a clean base data frame
  clean_df <- as.data.frame(analysis_obj)

  # Assign clean, standardized column names suited for paper tables
  colnames(clean_df) <- c("Item", "Alpha_If_Deleted", "Total_Correlation", "Discrimination")

  # Clear row indices for tidy print vectors
  rownames(clean_df) <- NULL

  # 5. Fetch overall alpha safely from attributes
  overall_alpha <- attr(analysis_obj, "cronbachs_alpha")

  # 6. CRITICAL FIX: Manually compute Mean Inter-Item Correlation safely
  # This extracts the off-diagonal elements of the item correlation matrix
  cor_matrix <- cor(items_subset, use = "pairwise.complete.obs")
  mean_iic   <- mean(cor_matrix[lower.tri(cor_matrix)])

  # Print high-level scale metrics as a clean console header
  cat("====================================================\n")
  cat("SCALE RELIABILITY SUMMARY\n")
  cat("====================================================\n")
  cat(paste0("Overall Scale Cronbach's Alpha: ",
             if(!is.null(overall_alpha)) round(overall_alpha, 3) else "NA", "\n"))
  cat(paste0("Mean Inter-Item Correlation:    ",
             if(!is.null(mean_iic)) round(mean_iic, 3) else "NA", "\n"))
  cat("====================================================\n\n")

  return(clean_df)
}
