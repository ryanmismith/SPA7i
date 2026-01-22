#' Simulate Annual Allowable Cut (AAC) growth over multiple years.
#'
#' @description This function simulates the AAC growth over a specified number of years for hardwood (HW)
#' and softwood (SW) volumes. It updates the volumes annually based on growth and AAC values.
#'
#' Default `maxvol`, `min_rate`, `sw_max_rate`, `hw_max_rate` from\code{\link{calculate_aac}} are used.
#' `min_rate` of .5 cords AAC is established once growth rates are below .25 cords/year.
#'
#' @param hw_volume Numeric, initial hardwood volume (required).
#' @param sw_volume Numeric, initial softwood volume (required).
#' @param aac_percentage Numeric, AAC percentage of growth until min stocking is reached (required).
#' @param min_stocking Numeric, minimum stocking level for AAC to begin or to reduce AAC to growth (required).
#' @param max_harvest Logical, whether to apply maximum harvest to `min_stocking` at period of first entry (default is FALSE).
#' @param min_aac Logical, whether to apply minimum AAC > growth once growth rates slow due to density (default is TRUE).
#' @param harvest_mode Character. "percentage" (default) or "concentrated".
#' @param removal Numeric. The fixed amount (cords/acre) to harvest in "concentrated" mode.
#' @param years Integer, number of years to simulate (default is 20).
#'
#' @return A dataframe with AAC results for each year, including standing HW/SW/Total volumes,
#' HW/SW/Total AAC, and HW/SW/Total growth rates.Year 0 shows starting volumes before any growth or harvest.
#'
#' @family AAC Functions
#'
#'
#' @examples
#' # Scenario 1: HW = 11, SW = 9, max_harvest = TRUE
#' results_scenario_1 <- run_aac_simulation(hw_volume = 11, sw_volume = 9,
#'                                          aac_percentage = 0.6, min_stocking = 12,
#'                                          max_harvest = TRUE)
#' print(results_scenario_1)
#'
#' # Scenario 2: HW = 11, SW = 9, max_harvest = FALSE
#' results_scenario_2 <- run_aac_simulation(hw_volume = 11, sw_volume = 9,
#'                                          aac_percentage = 0.6, min_stocking = 12,
#'                                          max_harvest = FALSE)
#' print(results_scenario_2)
#'
#' # Scenario 3 (min_aac functionality): HW = 21, SW = 21, max_harvest = FALSE
#' results_scenario_3 <- run_aac_simulation(hw_volume = 21, sw_volume = 21,
#'                                          aac_percentage = 0.6, min_stocking = 12,
#'                                          max_harvest = FALSE)
#' print(results_scenario_3)
#'
#' # Scenario 4 (no min_aac): HW = 21, SW = 21, max_harvest = FALSE, min_aac = FALSE
#' results_scenario_4 <- run_aac_simulation(hw_volume = 21, sw_volume = 21,
#'                                          aac_percentage = 0.6, min_stocking = 12,
#'                                          max_harvest = FALSE, min_aac = FALSE)
#' print(results_scenario_4)
#'
#' # Scenario 5 (no initial AAC): HW = 5, SW = 5, all defaults except min_stocking = 12
#' results_scenario_5 <- run_aac_simulation(hw_volume = 5, sw_volume = 5,
#'                                          aac_percentage = 0.6, min_stocking = 12,
#'                                          max_harvest = FALSE)
#' print(results_scenario_5)
#' @export

run_aac_simulation <- function(township = 'T13R5',
                               hw_volume, sw_volume,
                               aac_percentage, min_stocking,
                               max_harvest = FALSE, min_aac = TRUE,
                               harvest_mode = "percentage", removal = 10,
                               years = 20) {

  maxVol <- calculate_max_volume(hw_volume, sw_volume)
  total_volume <- hw_volume + sw_volume

  results <- data.frame(
    Year         = 0,
    HW_Volume    = hw_volume,
    SW_Volume    = sw_volume,
    Total_Volume = total_volume,
    HW_AAC       = NA_real_,
    SW_AAC       = NA_real_,
    Total_AAC    = NA_real_,
    total_growth = NA_real_,
    hw_growth    = NA_real_,
    sw_growth    = NA_real_,
    Entry_Status = 0
  )

  for (year in 1:years) {
    # One-time max harvest in period 1 only
    apply_max_harvest <- isTRUE(max_harvest) && year == 1

    aac_result <- calculate_aac(
      township      = township,
      hw_volume     = hw_volume,
      sw_volume     = sw_volume,
      aac_percentage = aac_percentage,
      min_stocking  = min_stocking,
      max_harvest   = apply_max_harvest,
      min_aac       = min_aac,
      maxvol        = maxVol,
      harvest_mode   = harvest_mode,       # NEW ARG
      removal = removal    # NEW ARG
    )

    # Update standing volumes
    hw_volume <- max(0, hw_volume + aac_result$hw_growth - aac_result$hw_aac)
    sw_volume <- max(0, sw_volume + aac_result$sw_growth - aac_result$sw_aac)
    total_volume <- hw_volume + sw_volume

    results <- rbind(
      results,
      data.frame(
        Year         = year,
        HW_Volume    = hw_volume,
        SW_Volume    = sw_volume,
        Total_Volume = total_volume,
        HW_AAC       = aac_result$hw_aac,
        SW_AAC       = aac_result$sw_aac,
        Total_AAC    = aac_result$hw_aac + aac_result$sw_aac,
        total_growth = aac_result$growth_rate,
        hw_growth    = aac_result$hw_growth,
        sw_growth    = aac_result$sw_growth,
        Entry_Status = as.integer(aac_result$entry_made) # Track 1 for entry, 0 for no entry
      )
    )
  }

  return(results)
}

