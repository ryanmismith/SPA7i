#' Run AAC simulations for all townships and matrices.
#'
#' @description This function runs the AAC simulation for each unique township-matrix combination in the dataset.
#' Data format must match the export from ArcGIS using the SPA analysis tool. ArcGIS output should be loaded unaltered.
#'
#' Default `maxvol`, `min_rate`, `sw_max_rate`, `hw_max_rate` from\code{\link{calculate_aac}} are used.
#' `min_rate` of .5 cords AAC is established once growth rates are below .25 cords/year.
#' `hw_volume` and `sw_volume` arguments for \code{\link{run_aac_simulation}} extracted from ArcGIS output.
#'
#' The same `aac_percentage`, `min_stocking`, `max_harvest`, `min_aac` arguments will be applied to all strata.
#' This can result in behavior where some strata are not harvested until they reach minimum stocking while others have
#' a `max_harvest` performed in period 1.
#'
#' Consult documentation for \code{\link{run_aac_simulation}} for further clarification on annualized runs and return values.
#'
#' @param df A dataframe containing the volume data, including columns for 'perAcreHW' and 'perAcreSW' (required).
#' @param aac_percentage Numeric, AAC percentage of growth until min stocking is reached (required).
#' @param min_stocking Numeric, minimum stocking level for AAC to begin or to reduce AAC to growth (required).
#' @param max_harvest Logical, whether to apply maximum harvest to `min_stocking` at period of first entry (default is FALSE).
#' @param min_aac Logical, whether to apply minimum AAC > growth once growth rates slow due to density (default is TRUE).
#' @param harvest_mode Character. "percentage" (default) or "concentrated".
#' @param removal Numeric. The fixed amount (cords/acre) to harvest in "concentrated" mode.
#' @param years Integer, number of years to simulate (default is 20).
#'
#' @return A dataframe with the growth and AAC simulation results for all township-matrix combinations over the specified years.
#'
#' @family AAC Functions
#' @seealso \code{\link{calculate_aac}}, \code{\link{run_aac_simulation}}
#' @importFrom dplyr bind_rows group_by group_map
#' @examples
#' # Simulated data
#' vols_wide <- data.frame(
#'   township = c("township1", "township1", "township2", "township2"),
#'   matrix = c("matrix1", "matrix2", "matrix1", "matrix2"),
#'   perAcreHW = c(12, 11, 0, 2),
#'   perAcreSW = c(11, 5, 20, 14)
#' )
#'
#' # Example usage
#' results_combined <- run_aac_for_all(vols_wide, aac_percentage = 1,
#' min_stocking = 12, years = 30, max_harvest = TRUE)
#' print(results_combined)
#'
#'
#' @export

run_aac_for_all <- function(df, aac_percentage, min_stocking,
                            max_harvest = FALSE, min_aac = TRUE, years = 20,
                            harvest_mode = "percentage", removal = 10) {

  # Ensure numeric values for aac_percentage and min_stocking
  aac_percentage <- as.numeric(aac_percentage)
  min_stocking <- as.numeric(min_stocking)

  if(!"Acres" %in% names(df)) stop("Input dataframe must have an 'Acres' column to calculate harvested acres.")

  df |>
    dplyr::group_by(township, matrix) |>
    dplyr::group_map(~ {
      # Process each group with scalar values
      twp       <- .x$township[1]
      hw_volume <- as.numeric(.x$perAcreHW[1])
      sw_volume <- as.numeric(.x$perAcreSW[1])

      # Determine acres for this specific stratum
      # Using sum() in case the group has multiple rows (though usually 1 per matrix)
      stratum_acres <- sum(as.numeric(.x$Acres), na.rm = TRUE)

      # Run the AAC simulation for each group
      result <- run_aac_simulation(township = twp,
                                   hw_volume, sw_volume, aac_percentage, min_stocking,
                                   max_harvest = max_harvest, min_aac = min_aac, years = years,
                                   harvest_mode = harvest_mode, removal = removal)

      # Add township and matrix information to the result
      result$township <- unique(.x$township)
      result$matrix <- unique(.x$matrix)
      result$Acres <- stratum_acres
      result$Harvested_Acres <- result$Entry_Status * stratum_acres

      return(result)
    }, .keep = TRUE) |>
    dplyr::bind_rows()
}
