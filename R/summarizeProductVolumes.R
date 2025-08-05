#' Summarize Product Volumes for Initial and Final Years
#'
#' @description This function takes the results of the product volume calculations and
#' aggregates the standing and harvest volumes for specified wood groupings
#' (hardwood, spruce-fir, cedar, pine, other softwood, poplar) for both initial and final years.
#' It also calculates the percentage of sawlogs and pulp for each year in the hardwood species group.
#'
#' The function returns three dataframes:
#' 1. `annual_volumes`: Summarized standing and harvest volumes for each species group across all years.
#' 2. `summary_volumes`: Summarized standing and harvest volumes for the first and final years.
#' 3. `hardwood_volumes`: Detailed breakdown of sawlog and pulp volumes and percentages for hardwood.
#'
#' @param aac_results A dataframe containing AAC results for multiple years.
#' @param ratios_df A dataframe containing the ratios for hardwood and softwood for each township and matrix.
#' @param years Integer, number of years to calculate product volumes for (default is 20).
#'
#' @return A list containing three dataframes:
#' 1. `annual_volumes`: Aggregated standing and harvest volumes for each species group across all years.
#' 2. `summary_volumes`: Summarized standing and harvest volumes for the first and final years.
#' 3. `hardwood_volumes`: Detailed breakdown of sawlog and pulp volumes and percentages for the hardwood species group.
#'
#' @family Reporting Functions
#' @seealso \code{\link{calculateProductRatios}} and \code{\link{run_aac_for_all}} to create `aac_results` and `ratios_df`.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' summary_volumes <- summarizeProductVolumes(aac_results, ratios_df)
#' print(summary_volumes$annual_volumes)
#' print(summary_volumes$summary_volumes)
#' print(summary_volumes$hardwood_volumes)
#' }
#' @export

summarizeProductVolumes <- function(aac_results, ratios_df, years = 20) {

  # Call calculateProductVolumes to get the product volumes dataframe
  product_volumes <- calculateProductVolumes(aac_results, ratios_df, years = years)

  # Wood type mapping for both types of categories
  wood_type_mapping <- c(
    "ASl" = "HW", "ASp" = "HW", "BFl" = "SW", "BFp" = "SW",
    "CEl" = "SW", "CEp" = "SW", "HVl" = "HW", "HVp" = "HW",
    "HVt" = "HW", "LVl" = "HW", "LVp" = "HW", "LVt" = "HW",
    "OSl" = "SW", "OSp" = "SW", "SPl" = "SW", "SPp" = "SW",
    "WPl" = "SW", "WPp" = "SW"
  )

  # Grouping for aggregation
  groupings <- c(
    "ASl" = "Poplar", "ASp" = "Poplar", "BFl" = "Spruce-Fir", "BFp" = "Spruce-Fir",
    "CEl" = "Cedar", "CEp" = "Cedar", "HVl" = "Hardwood", "HVp" = "Hardwood",
    "HVt" = "Hardwood", "LVl" = "Hardwood", "LVp" = "Hardwood", "LVt" = "Hardwood",
    "OSl" = "Other Softwood", "OSp" = "Other Softwood", "SPl" = "Spruce-Fir", "SPp" = "Spruce-Fir",
    "WPl" = "Pine", "WPp" = "Pine"
  )


  # Add wood type mapping and groupings to product_volumes
  product_volumes <- product_volumes |>
    dplyr::mutate(
      wood_type = wood_type_mapping[product],
      species_group = groupings[product],
      product_class = dplyr::case_when(
        endsWith(product, "l") | endsWith(product, "t") ~ "sawlog",
        endsWith(product, "p") ~ "pulp",
        TRUE ~ "other"
      )
    )

  # Get annual standing and harvest volumes
  annual_volumes <- product_volumes |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarize(total_volume = sum(product_standing_volume, na.rm = TRUE),
                     total_harvest = sum(product_harvest_volume, na.rm = TRUE)) |>
    dplyr::filter(total_volume > 0) |> dplyr::ungroup()

  # Hardwood Sawlogs to Pulp
  hardwood_volumes <- product_volumes |>
    filter(species_group == "Hardwood") |>
    dplyr::group_by(year, species_group, product_class) |>
    dplyr::summarize(
      total_volume = sum(product_standing_volume, na.rm = TRUE),
      total_harvest = sum(product_harvest_volume, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    # Calculate total volumes for sawlog and pulp within each year
    dplyr::group_by(year, species_group) |>
    dplyr::mutate(
      total_sawlog_volume = sum(total_volume[product_class == "sawlog"], na.rm = TRUE),
      total_pulp_volume = sum(total_volume[product_class == "pulp"], na.rm = TRUE),
      total_sawlog_harvest = sum(total_harvest[product_class == "sawlog"], na.rm = TRUE),
      total_pulp_harvest = sum(total_harvest[product_class == "pulp"], na.rm = TRUE)
    ) |>
    # Calculate product_percent and harvest_percent
    dplyr::mutate(
      product_percent = dplyr::case_when(
        product_class == "sawlog" ~ total_volume / (total_sawlog_volume + total_pulp_volume),
        product_class == "pulp" ~ total_volume / (total_sawlog_volume + total_pulp_volume),
        TRUE ~ NA_real_
      ),
      harvest_percent = dplyr::case_when(
        product_class == "sawlog" ~ total_harvest / (total_sawlog_harvest + total_pulp_harvest),
        product_class == "pulp" ~ total_harvest / (total_sawlog_harvest + total_pulp_harvest),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    mutate(group = "hardwood") |>
    select(group, year, product_class, total_volume, total_harvest, product_percent, harvest_percent)

  # Get the first year (1) and final year (where sum(product_standing_volume) > 0)
  first_year <- 2
  final_year <- product_volumes |>
    dplyr::group_by(year) |>
    dplyr::summarize(total_volume = sum(product_standing_volume, na.rm = TRUE)) |>
    dplyr::filter(total_volume > 0) |>
    dplyr::pull(year) |>
    max()

  # Summarize for the initial year
  initial_summary <- product_volumes |>
    dplyr::filter(year == first_year) |>
    dplyr::group_by(species_group) |>
    dplyr::summarize(
      start_year = max(year),
      initial_volume = round(sum(product_standing_volume, na.rm = TRUE), 0),
      initial_harvest = round(sum(product_harvest_volume, na.rm = TRUE), 1)
    )

  # Summarize for the final year
  final_summary <- product_volumes |>
    dplyr::filter(year == final_year) |>
    dplyr::group_by(species_group) |>
    dplyr::summarize(
      final_year = max(year),
      final_volume = round(sum(product_standing_volume, na.rm = TRUE), 0),
      final_harvest = round(sum(product_harvest_volume, na.rm = TRUE), 1)
    )

  # Merge the initial and final summaries
  summary_volumes <- dplyr::full_join(initial_summary, final_summary, by = "species_group")

  return(list(annual_volumes = annual_volumes, summary_volumes = summary_volumes, hardwood_volumes = hardwood_volumes))

}
