#' Calculate product volumes based on AAC results and ratios.
#'
#' @description This function calculates the product harvest and standing volumes for each township and matrix
#' based on the AAC results and hardwood/softwood ratios.
#'
#' @param aac_results A dataframe containing AAC results for multiple years.
#' @param ratios_df A dataframe containing the ratios for hardwood and softwood for each township and matrix.
#' @param years Integer, number of years to calculate product volumes for (default is 20).
#'
#' @return A dataframe containing the product harvest volume and standing volume for each township and matrix over the specified years.
#'
#' @examples
#' # Simulated AAC results
#' aac_results <- data.frame(
#'   township = c("township1", "township1", "township2"),
#'   matrix = c("matrix1", "matrix2", "matrix1"),
#'   Year = c(1, 1, 1),
#'   HW_AAC = c(40, 40, 100),
#'   SW_AAC = c(25, 80, 100),
#'   HW_Volume = c(1000, 1200, 1500),
#'   SW_Volume = c(500, 800, 600)
#' )
#'
#' # Simulated ratios dataframe
#' ratios_df <- data.frame(
#'   township = c("township1", "township1", "township2"),
#'   matrix = c("matrix1", "matrix2", "matrix1"),
#'   wood_type = c("HW", "SW", "HW"),
#'   HWratio = c(0.6, 0.7, 0.8),
#'   SWratio = c(0.4, 0.3, 0.2),
#'   acres = c(10, 20, 15)
#' )
#'
#' @family Product Functions
#'
#' # Example usage
#' product_volumes <- calculate_product_volumes(aac_results, ratios_df, years = 20)
#' print(product_volumes)
#' @export

calculate_product_volumes <- function(aac_results, ratios_df, years = 20) {
  final_results <- data.frame()

  # Convert column names in aac_results to lowercase
  colnames(aac_results) <- tolower(colnames(aac_results))
  colnames(ratios_df) <- tolower(colnames(ratios_df))
  for (year in 1:years) {

    # Extract AAC values and volumes for the current year
    year_aac <- aac_results |>
      dplyr::filter(year == !!year) |>
      dplyr::select(township, matrix, hw_aac, sw_aac, hw_volume, sw_volume)

    # Merge the AAC values with the product ratios and acres
    merged_df <- ratios_df |>
      dplyr::left_join(year_aac, by = c("township", "matrix")) |>
      dplyr::group_by(township, matrix, product, wood_type, acres) |>
      dplyr::mutate(
        # Calculate total AAC for HW and SW per township/matrix based on acres
        total_hw_aac = hw_aac * acres,
        total_sw_aac = sw_aac * acres,

        # Calculate the harvest volume for each product based on HW or SW AAC
        product_harvest_volume = ifelse(wood_type == "HW",
                                        hwratio * total_hw_aac,  # HW product harvest volume
                                        swratio * total_sw_aac),  # SW product harvest volume

        # Calculate the total standing volume for each product based on HW or SW standing volume
        product_standing_volume = ifelse(wood_type == "HW",
                                         hwratio * hw_volume * acres,  # HW product standing volume
                                         swratio * sw_volume * acres)  # SW product standing volume
      ) |>
      dplyr::ungroup()

    # Add the year information
    merged_df$year <- year

    # Select the relevant columns (including the new standing volume and harvest volume)
    merged_df <- merged_df |>
      dplyr::select(township, matrix, product, wood_type, acres, year,
                    product_harvest_volume, product_standing_volume) |>
      dplyr::distinct()  # Remove any duplicates after calculations

    # Append the result for this year
    final_results <- dplyr::bind_rows(final_results, merged_df)
  }

  return(final_results)
}
