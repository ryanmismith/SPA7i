#' Load and prepare data for analysis from an Excel file.
#'
#' @description This function processes an Excel file with two sheets:
#' "Inventory" and "Spatial". The "Inventory" sheet is cleaned by selecting
#' relevant columns (`township`, `matrix`, `acres`, `product`, `cords`), and any
#' column containing the word "matrix" is renamed to "matrix" if not already named so.
#' The "Spatial" sheet has all its colnames converted to lowercase.
#'
#' @param filepath A string representing the file path to the Excel file containing the data.
#'
#' @return A named list with two dataframes: "Inventory" and "Spatial".
#' The "Inventory" dataframe contains selected and cleaned data from the "Inventory" sheet.
#' The "Spatial" dataframe contains all lowercase text from the "Spatial" sheet.
#' @family InputTransform Functions
#' @importFrom readxl read_excel
#' @importFrom dplyr select rename mutate across
#' @examples
#' \dontrun{
#' # Example usage with an Excel file containing "Inventory" and "Spatial" sheets.
#' data_list <- dataPreparation("path_to_excel_file.xlsx")
#' inventory_data <- data_list$Inventory
#' spatial_data <- data_list$Spatial
#' print(inventory_data)
#' print(spatial_data)
#' }
#' @export

dataPreparation <- function(filepath) {
  # Process the 'Inventory' sheet
  inventory_data <- read_excel(filepath, sheet = "Inventory") %>%
    # Remove unnamed columns and convert column names to lowercase
    select(where(~ !all(is.na(.)))) |>
    rename_with(tolower) %>%
    # Check if any column contains "matrix" and is not already "matrix"
    {
      matrix_cols <- grep("matrix", colnames(.), value = TRUE)
      if (length(matrix_cols) > 0 && !"matrix" %in% colnames(.)) {
        . <- rename(., matrix = matrix_cols[1])
      }
      .
    } %>%
    # Select relevant columns
    select(township, matrix, acres, product, cords)

  # Process the 'Spatial' sheet
  spatial_data <- read_excel(filepath, sheet = "Spatial") %>%
    # Convert all text to lowercase
    rename_with(tolower)

  # Return as a named list
  return(list(
    Inventory = inventory_data,
    Spatial = spatial_data
  ))
}
