#' Load and prepare data for analysis from an Excel file.
#'
#' @description This function processes an Excel file with two sheets:
#' "Inventory" and "Spatial". The "Inventory" sheet is cleaned by selecting
#' relevant columns (`township`, `matrix`, `acres`, `product`, `cords`), and any
#' column containing the word "matrix" is renamed to "matrix" if not already named so.
#' If `landuse` is present, it will also be passed on in the inventory table but it is not required.
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
    select(where(~ !all(is.na(.)))) %>%   # Drop all-NA columns
    rename_with(tolower) %>%              # Lowercase names
    {
      matrix_cols <- grep("matrix", colnames(.), value = TRUE)
      if (length(matrix_cols) > 0 && !"matrix" %in% colnames(.)) {
        rename(., matrix = matrix_cols[1])
      } else .
    } %>%
    {
      if (!"unit" %in% names(.)) {
        message("No 'unit' column found. Assigning one with assign_spa_unit().")
        mutate(., unit = assign_spa_unit(township))
      } else .
    } %>%
    {
      # REQUIRED columns
      required_cols <- c("unit", "township", "matrix", "acres", "product", "cords")

      # OPTIONAL columns to keep ONLY if they exist
      optional_cols <- c("landuse")

      # Keep optional cols only if they exist in the dataset
      available_optional <- intersect(optional_cols, names(.))

      # Identify missing required ones as before
      missing_cols <- setdiff(required_cols, names(.))
      if (length(missing_cols) > 0) {
        warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
        for (col in missing_cols) .[[col]] <- NA
      }

      # Select required + any optional that exist
      select(., all_of(c(required_cols, available_optional)))
    }

  # Process Spatial sheet
  spatial_data <- read_excel(filepath, sheet = "Spatial") %>%
    rename_with(tolower)

  list(
    Inventory = inventory_data,
    Spatial = spatial_data
  )
}
