#' Prepare Growth Model Input Data.
#'
#' @description This function prepares the input data for growth models by renaming the relevant columns
#' in the provided dataframe that correspond to per-acre hardwood and softwood values.
#' It ensures that columns related to per-acre volumes for hardwood and
#' softwood are renamed to `perAcreHW` and `perAcreSW` respectively, and filters the
#' dataframe to include only the relevant products.
#' The function then pivots the dataframe to set it up for AAC simulations.
#'
#' @param arc_df The SPA export file from ArcGIS after loading with `data_preparation`.
#' A dataframe containing product data, including variables related to per-acre hardwood (HW) and softwood (SW) volumes.
#'
#' @return A dataframe with renamed columns for hardwood and softwood per-acre values, filtered to only include these columns, and pivoted. into the correct structure for further analysis.
#'
#' @examples
#' # Example dataframe
#' arc_df <- data.frame(
#'   township = c("township1", "township1", "township2", "township2"),
#'   matrix = c("matrix1", "matrix1", "matrix1", "matrix1"),
#'   product = c("PerAcreSW", "perAcreHW", "perAcreSW", "perAcre_HW"),
#'   cords = c(10, 15, 5, 8)
#'   )
#'
#' # Generate input for growth models
#' final_results <- GrowthModelInput(arc_df)
#' print(final_results)
#'
#' @family InputTransform Functions
#' @seealso \code{\link{calculate_aac}}, \code{\link{run_aac_simulation}}
#' @export


GrowthModelInput <- function(arc_df){
  # Create Empty Dataframe
  final_results <- data.frame()

  # Ensure Correct Variable Names
  arc_df_cleaned <- arc_df |>
    dplyr::mutate(
      product = dplyr::case_when(
        grepl("(?i)(hw.*acre|acre.*hw|hw[_]*acre|acre[_]*hw)", product) ~ "perAcreHW",  # Pattern for HW and Acre
        grepl("(?i)(sw.*acre|acre.*sw|sw[_]*acre|acre[_]*sw)", product) ~ "perAcreSW",  # Pattern for SW and Acre
        TRUE ~ product  # Keep the original value if it doesn't match HW or SW
      )
    )

  final_results <- arc_df_cleaned |>
    dplyr::filter(product %in% c('perAcreHW', 'perAcreSW'))

  #Pivot to get DF in correct structure for AAC Simulations
  final_results <- final_results |>
    tidyr::pivot_wider(
      names_from = product,   # Column to create new columns from
      values_from = cords     # Column containing the values for the new columns
      )

      return(final_results)
}


