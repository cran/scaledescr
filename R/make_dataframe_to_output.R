#' Export a data frame to CSV, Word, or Excel
#'
#' This function exports a data frame as a structured table to a file in CSV,
#' Word (`.docx`), or Excel (`.xlsx`) format.
#'
#' If no output path is specified, the file is written to a temporary directory
#' using `tempdir()`. For reproducible workflows, users are encouraged to
#' explicitly specify an output location, either the current working directory
#' or a full file path.
#'
#' - CSV files are written using `utils::write.csv()`
#' - Word (`docx`) files are created using the `officer` package
#' - Excel (`xlsx`) files are created using the `openxlsx` package
#'
#' @param x A data frame to export.
#' @param filename Optional character string specifying the base file name
#'   (without path and without extension). If not provided, the name of the
#'   input object is used.
#' @param format Character string specifying the output format. One of
#'   `"csv"`, `"docx"`, or `"excel"`. Default is `"csv"`.
#' @param path Optional character string specifying the directory where the
#'   file should be saved. If `NULL` (default), the file is written to a
#'   temporary directory. Use `path = "getwd"` to save the file in the current
#'   working directory, or provide a full directory path.
#'
#' @return Invisibly returns the full file path to the generated output file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3, y = c("A", "B", "C"))
#'
#' # 1. Default behaviour: write to a temporary directory (CRAN-safe)
#' make_dataframe_to_output(df, format = "csv")
#'
#' # 2. Write to the current working directory (recommended for projects)
#' make_dataframe_to_output(
#'   df,
#'   format = "csv",
#'   path = "getwd"
#' )
#'
#' # 3. Write to a user-specified directory
#' make_dataframe_to_output(
#'   df,
#'   filename = "demo_table",
#'   format = "excel",
#'   path = tempdir()
#' )
#' }
make_dataframe_to_output <- function(
  x,
  filename = NULL,
  format = "csv",
  path = NULL
) {
  if (!is.data.frame(x)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  ## Validate format
  format <- tolower(format)
  if (!format %in% c("csv", "docx", "excel")) {
    stop("Format must be one of 'csv', 'docx', or 'excel'.", call. = FALSE)
  }

  ## Default filename from object name
  if (is.null(filename)) {
    filename <- make.names(deparse(substitute(x)))
  }

  ## Determine output directory
  if (is.null(path)) {
    out_dir <- tempdir()
  } else if (identical(path, "getwd")) {
    out_dir <- getwd()
  } else {
    out_dir <- path
  }

  ## Check directory exists
  if (!dir.exists(out_dir)) {
    stop("Specified path does not exist: ", out_dir, call. = FALSE)
  }

  ## Construct full file path
  filepath <- file.path(out_dir, paste0(filename, ".", format))

  ## Write file
  if (format == "csv") {
    utils::write.csv(x, filepath, row.names = FALSE, fileEncoding = "UTF-8")
  } else if (format == "docx") {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Please install the 'officer' package to export Word documents.", call. = FALSE)
    }

    colnames(x) <- make.names(colnames(x))
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, filename, style = "heading 1")
    doc <- officer::body_add_table(doc, value = x)
    print(doc, target = filepath)
  } else if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Please install the 'openxlsx' package to export Excel files.", call. = FALSE)
    }

    openxlsx::write.xlsx(x, filepath, rowNames = FALSE)
  }

  message("File created: ", filepath)
  invisible(filepath)
}

