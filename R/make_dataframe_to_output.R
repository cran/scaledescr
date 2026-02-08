#' Export a data frame to CSV, Word, or Excel
#'
#' This function writes a data frame to a user-specified file path in a user-specified format (CSV, Word, or Excel).
#' - CSV uses `write.csv()`
#' - Word (`docx`) uses `officer` and makes column names safe
#' - Excel (`xlsx`) uses `openxlsx`
#'
#' @param x A data frame to export
#' @param filename Optional. The file name (with or without path, without extension).
#'                 If not provided, the data frame name is used.
#' @param format Character. One of `"csv"`, `"docx"`, or `"excel"`. Default is `"csv"`.
#'
#' @return Invisible path to the generated file
#' @export
#' @examples
#' \donttest{
#' df <- data.frame(x = 1:3, y = c("A", "B", "C"))
#'
#' # Write to a temporary directory (CRAN-safe)
#' tmp_file <- file.path(tempdir(), "demo_csv")
#' make_dataframe_to_output(df, filename = tmp_file, format = "csv")
#' }
make_dataframe_to_output <- function(x, filename = NULL, format = "csv") {
  if (!is.data.frame(x)) stop("Input must be a data frame")

  # Default filename from object
  if (is.null(filename)) {
    filename <- gsub("[[:punct:]\\s]+", "_", deparse(substitute(x)))
  }

  # Determine final file path
  if (!grepl("[:/\\\\]", filename)) {
    stop(
      "Please provide a full file path (CRAN policy forbids writing to the working directory by default).",
      call. = FALSE
    )
  } else {
    filepath <- filename
  }

  # Ensure extension matches format
  if (!grepl(paste0("\\.", format, "$"), filepath, ignore.case = TRUE)) {
    filepath <- paste0(filepath, ".", format)
  }

  # Write file
  if (format == "csv") {
    utils::write.csv(x, file = filepath, row.names = FALSE, fileEncoding = "UTF-8")
  } else if (format == "docx") {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Please install 'officer' to export Word documents.")
    }
    colnames(x) <- make.names(colnames(x))
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, basename(filepath), style = "heading 1")
    doc <- officer::body_add_table(doc, value = x)
    print(doc, target = filepath)
  } else if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Please install 'openxlsx' to export Excel files.")
    }
    openxlsx::write.xlsx(x, file = filepath, rowNames = FALSE)
  } else {
    stop("Format not recognized. Choose 'csv', 'docx', or 'excel'.")
  }

  message("File created: ", filepath)
  invisible(filepath)
}
