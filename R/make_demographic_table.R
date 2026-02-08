#' Create a demographics summary table
#'
#' @param data A data frame
#' @param vars demographic variables to include in the table
#' @param continuous_vars Optional subset of vars to be treated as continuous
#'
#' @return A gtsummary table
#' @export
#' @examples
#' # Minimal example data
#' df <- data.frame(
#'   age = c(25, 30, 35),
#'   sex = c("M", "F", "M"),
#'   education = c("HS", "BA", "MA")
#' )
#'
#' # Generate a demographic summary table (assign to object to avoid printing)
#' demo_table <- make_demographic_table(df, vars = c("age", "sex", "education"))
#' demo_table # optionally inspect the table
#'
#' @importFrom dplyr select all_of
#' @importFrom dplyr %>%
#' @importFrom gtsummary tbl_summary all_categorical all_continuous
#' @importFrom purrr map_lgl map_chr
#' @importFrom rlang enquos as_name
make_demographic_table <- function(data, vars, continuous_vars = NULL) {
  # --- FAST CHECK: all vars exist in data ---
  if (!all(vars %in% names(data))) {
    stop("Some variables in `vars` are not present in `data`.", call. = FALSE)
  }

  data_selected <- data |>
    dplyr::select(dplyr::all_of(vars))

  if (is.null(continuous_vars)) {
    # Auto-detect numeric variables
    continuous_names <- names(data_selected)[
      purrr::map_lgl(data_selected, is.numeric)
    ]
  } else {
    # User-specified continuous variables
    continuous_names <- continuous_vars

    # Coerce even if alphabets are present
    data_selected <- data_selected |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(continuous_names),
          ~ suppressWarnings(
            as.numeric(gsub("[^0-9.+-]", "", as.character(.)))
          )
        )
      )
  }

  gtsummary::tbl_summary(
    data_selected,
    statistic = list(
      gtsummary::all_categorical() ~ "{n} ({p}%)",
      gtsummary::all_continuous() ~ "{mean} ({sd})"
    ),
    type = if (length(continuous_names) > 0) {
      list(dplyr::all_of(continuous_names) ~ "continuous")
    },
    missing = "no"
  )
}
