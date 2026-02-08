#' Wrap a pre-computed psych::alpha object into a single-row table
#'
#' @param alpha_res A psych::alpha object (already computed)
#' @param scale_name Name of the scale (default: "Scale")
#'
#' @return A data frame with columns: Scale, 95% CI lower, Alpha, 95% CI upper
#' @examples
#' # Create a minimal "psych::alpha" like object manually
#' alpha_obj <- list(
#'   total = list(
#'     raw_alpha = 0.85,
#'     lower = 0.78,
#'     upper = 0.92
#'   )
#' )
#'
#' # Generate the formatted alpha table
#' make_alpha_table(alpha_obj, scale_name = "PHQ-9")

#' @export
make_alpha_table <- function(alpha_res, scale_name = "Scale") {
  # Extract raw alpha and round to 2 decimals
  raw_alpha <- round(alpha_res$total$raw_alpha, 2)

  # Extract Feldt 95% CI (NA if not available) and round to 2 decimals
  ci_lower <- if (length(alpha_res$total$lower) > 0) round(as.numeric(alpha_res$total$lower), 2) else NA
  ci_upper <- if (length(alpha_res$total$upper) > 0) round(as.numeric(alpha_res$total$upper), 2) else NA

  # Single-row table
  data.frame(
    Scale = scale_name,
    `95% CI lower` = ci_lower,
    Alpha = raw_alpha,
    `95% CI upper` = ci_upper,
    check.names = FALSE
  )
}
