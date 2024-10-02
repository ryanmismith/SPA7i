#' Load and prepare the data for analysis.
#'
#' @description This function loads volume data from a CSV file, converts the column names to lowercase,
#' and renames any column containing the word "matrix" to "matrix", if it's not already named "matrix".
#'
#' @param vols_path A string representing the file path to the CSV file containing volume data.
#'
#' @return A cleaned dataframe with lowercase column names and the renamed 'matrix' column.
#' @family InputTransform Functions
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' # Example 1: Column named 'lumpmatrix'
#' simulated_data_1 <- data.frame(
#'   lumpmatrix = c("matrix1", "matrix2", "matrix1"),
#'   product = c("perAcreHW", "perAcreSW", "perAcreHW"),
#'   cords = c(100, 50, 200),
#'   township = c("township1", "township1", "township2"),
#'   acres = c(10, 20, 15)
#' )
#' # Save to CSV in a temporary directory
#' temp_file_1 <- file.path(tempdir(), "SPA_test1.csv")
#' write.csv(simulated_data_1, temp_file_1, row.names = FALSE)
#'
#' # Example usage where 'lumpmatrix' gets renamed to 'matrix'
#' vols_cleaned_1 <- data_preparation(temp_file_1)
#' print(vols_cleaned_1)
#'
#' # Example 2: Column already named 'MATRIX'
#' simulated_data_2 <- data.frame(
#'   MATRIX = c("matrix1", "matrix2", "matrix1"),
#'   product = c("perAcreHW", "perAcreSW", "perAcreHW"),
#'   cords = c(100, 50, 200),
#'   township = c("township1", "township1", "township2"),
#'   acres = c(10, 20, 15)
#' )
#' # Save to CSV in a temporary directory
#' temp_file_2 <- file.path(tempdir(), "SPA_test2.csv")
#' write.csv(simulated_data_2, temp_file_2, row.names = FALSE)
#'
#' # Example usage where 'MATRIX' is already 'matrix' and remains unchanged
#' vols_cleaned_2 <- data_preparation(temp_file_2)
#' print(vols_cleaned_2)
#' }
#' @export
data_preparation <- function(vols_path) {
  # Read CSV file
  vols <- read.csv(vols_path)

  # Convert all column names to lowercase
  colnames(vols) <- tolower(colnames(vols))

  # Check if any column contains "matrix" and is not already "matrix"
  matrix_cols <- grep("matrix", colnames(vols), value = TRUE)

  # If a column is found and it's not named "matrix", rename it to "matrix"
  if (length(matrix_cols) > 0 && !"matrix" %in% colnames(vols)) {
    vols <- vols |>
      rename(matrix = matrix_cols[1])  # Rename the first matching "matrix" column to "matrix"
  }

  return(vols)
}
