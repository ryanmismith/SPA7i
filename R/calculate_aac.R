#' Calculate Annual Allowable Cut (AAC)
#'
#' @description This function calculates the Annual Allowable Cut (AAC) for a given stand based on its hardwood (HW) and softwood (SW) volumes,
#' growth rates, and desired cutting percentages. It accounts for minimum stocking levels, allows for a one-time maximum harvest to reduce the volume
#' to the minimum stocking level, and ensures sustainable harvest practices based on growth rates. It also includes an optional minimum AAC functionality
#' to prevent overstocking in cases where growth rates are particularly low.
#'
#' @details
#' This function calculates the AAC for hardwood and softwood volumes, factoring in growth rates and ensuring the total volume never drops below a specified minimum stocking level.
#' The function uses the \code{\link{calculate_growth_rate}} function to calculate the growth rates for HW and SW, which itself uses the \code{\link{calculate_max_volume}} function
#' to compute the maximum allowable volume based on the proportion of HW and SW.
#'
#' The optional argument \code{max_harvest} allows for a one-time aggressive cut that reduces the total volume to the \code{min_stocking} level in the first year, after which
#' the function reverts to sustainable cutting based on the growth rate and applied AAC percentage. The growth rates and maximum volume calculations are based on the following defaults:
#'
#' - \code{sw_max_rate = 0.45} for softwood
#' - \code{hw_max_rate = 0.40} for hardwood
#' - \code{min_rate = 0.02} minimum growth rate
#' - \code{maxvol = 38} default maximum volume, which can be overridden
#'
#' The optional argument \code{min_aac} introduces a safeguard for very low growth rates. If the total growth for the stand falls below 0.25 cords/acre, the function will increase the total AAC to 0.5 cords/acre to avoid overstocking and improve future growth. If this behavior is undesirable, it can be turned off by setting \code{min_aac = FALSE}.
#'
#' ### Behavior when \code{max_harvest = TRUE}:
#' When \code{max_harvest = TRUE}, the function calculates a one-time cut that brings the total volume down to the minimum stocking level (\code{min_stocking}),
#' provided the total volume exceeds this threshold. In subsequent years, the function reverts to cutting based on growth rates and the applied \code{aac_percentage}.
#'
#' ### Behavior when \code{max_harvest = FALSE}:
#' In this case, the function cuts the total volume based on the specified \code{aac_percentage} while ensuring the volume
#' does not drop below the \code{min_stocking}. If applying the full AAC would cause the volume to drop below the \code{min_stocking},
#' the function reduces the cut to only 100% of the growth rate to maintain sustainable harvesting.
#'
#' ### Behavior when \code{min_aac = TRUE}:
#' If \code{min_aac = TRUE}, the function ensures that when the growth rate falls below 0.25 cords/acre, the total AAC is adjusted to 0.5 cords/acre. This helps to improve
#' future growth by preventing overstocking. If the user prefers to avoid this adjustment, they can set \code{min_aac = FALSE}. #
#'
#' @param hw_volume Numeric. The current volume of hardwood (e.g., cords/acre).
#' @param sw_volume Numeric. The current volume of softwood (e.g., cords/acre).
#' @param aac_percentage Numeric. The percentage of growth to be harvested, e.g., 2 for 200\%.
#' @param min_stocking Numeric. The minimum volume below which no AAC can be applied, default is 12 cords/acre.
#' @param max_harvest Logical. Whether to allow a one-time maximum harvest to reduce volume to \code{min_stocking}, default is \code{FALSE}.
#' @param min_aac Logical. Increases total AAC to 0.5 cords/acre if growth dips below 0.25 cords/acre, default is \code{TRUE}.
#' @param sw_max_rate Numeric. The initial growth rate for softwood, default is 0.45.
#' @param hw_max_rate Numeric. The initial growth rate for hardwood, default is 0.40.
#' @param min_rate Numeric. The minimum growth rate when the combined volume reaches or exceeds the maximum, default is 0.02.
#' @param maxvol Numeric. Default is 38. This value is used to calculate max volume before min growth using \code{\link{calculate_max_volume}}.
#'
#' @return A list containing:
#'
#' - `hw_aac`: The AAC for hardwood (cords/acre).
#'
#' - `sw_aac`: The AAC for softwood (cords/acre).
#'
#' - `growth_rate`: The combined growth rate for the stand (cords/acre/year).
#'
#' - `hw_growth`: The prorated growth for hardwood (cords/acre/year).
#'
#' - `sw_growth`: The prorated growth for softwood (cords/acre/year).
#'
#' @family AAC Functions
#' @seealso \code{\link{run_aac_simulation}}, \code{\link{calculate_max_volume}}, \code{\link{calculate_growth_rate}}
#' @examples
#' ### Single Year Calculations
#'
#' #HW
#' calculate_aac(hw_volume = 22, sw_volume = 0, aac_percentage = 2, min_stocking = 12,
#' max_harvest = FALSE, min_aac = TRUE,
#' sw_max_rate = .45, hw_max_rate = .40,
#' min_rate = .02, maxvol = 38)
#'
#' #SW
#' calculate_aac(hw_volume = 0, sw_volume = 22, aac_percentage = .5 , min_stocking = 12,
#' max_harvest = FALSE, min_aac = TRUE,
#' sw_max_rate = .45, hw_max_rate = .40,
#' min_rate = .02, maxvol = 42)
#'
#' #MW
#' calculate_aac(hw_volume = 13, sw_volume = 11, aac_percentage = 1, min_stocking = 14,
#' max_harvest = TRUE, min_aac = TRUE,
#' sw_max_rate = .45, hw_max_rate = .40,
#' min_rate = .02, maxvol = 40)
#'
#'
#' ### Function to run the AAC simulation over multiple periods for different scenarios
#' run_aac_simulation <- function(hw_volume, sw_volume, aac_percentage, min_stocking,
#'                                max_harvest, min_aac = TRUE, years = 15) {
#'   results <- data.frame(Year = numeric(), HW_Volume = numeric(), SW_Volume = numeric(),
#'                         Total_Volume = numeric(), HW_AAC = numeric(), SW_AAC = numeric(),
#'                         Total_AAC = numeric(), total_growth = numeric(),
#'                         hw_growth = numeric(), sw_growth = numeric())
#'
#'   for (year in 1:years) {
#'     # Calculate AAC for the year
#'     aac_result <- calculate_aac(hw_volume, sw_volume, aac_percentage, min_stocking,
#'                                 max_harvest, min_aac)
#'
#'     # Update volumes for next year, ensuring no negative or NaN volumes
#'     hw_volume <- hw_volume + aac_result$hw_growth - aac_result$hw_aac
#'     sw_volume <- sw_volume + aac_result$sw_growth - aac_result$sw_aac
#'
#'     # Ensure volumes stay positive
#'     hw_volume <- max(0, hw_volume)
#'     sw_volume <- max(0, sw_volume)
#'
#'     # Recalculate total volume
#'     total_volume <- hw_volume + sw_volume
#'
#'     # Store the results
#'     results <- rbind(results, data.frame(
#'       Year = year,
#'       HW_Volume = hw_volume,
#'       SW_Volume = sw_volume,
#'       Total_Volume = total_volume,
#'       HW_AAC = aac_result$hw_aac,
#'       SW_AAC = aac_result$sw_aac,
#'       Total_AAC = aac_result$hw_aac + aac_result$sw_aac,
#'       total_growth = aac_result$growth_rate,
#'       hw_growth = aac_result$hw_growth,
#'       sw_growth = aac_result$sw_growth
#'     ))
#'   }
#'   return(results)
#' }
#'
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
#' @seealso \code{\link{calculate_growth_rate}}, \code{\link{calculate_max_volume}}
#' @export


# Function to calculate the Annual Allowable Cut (AAC)
calculate_aac <- function(hw_volume, sw_volume, aac_percentage, min_stocking = 12,
                          max_harvest = FALSE, min_aac = TRUE,
                          sw_max_rate = .45, hw_max_rate = .40,
                          min_rate = .02, maxvol = 38) {
  total_volume <- hw_volume + sw_volume

  # Ensure no zero or negative volumes
  if ((hw_volume + sw_volume) <= 0) {
    hw_volume = .01
    sw_volume = .01
  }

  # Calculate HW and SW growth rates using calculate_growth_rate function
  growth_rate <- calculate_growth_rate(hw_volume = hw_volume, sw_volume = sw_volume,
                                       sw_max_rate = sw_max_rate, hw_max_rate = hw_max_rate,
                                       min_rate = min_rate, maxvol = maxvol)

  # Calculate the ratio of HW and SW based on the total volume
  hw_ratio <- if (hw_volume == 0) 0 else hw_volume / total_volume
  sw_ratio <- if (sw_volume == 0) 0 else sw_volume / total_volume

  # Prorate the growth rates by their respective volume ratios
  prorated_hw_growth <- growth_rate * hw_ratio
  prorated_sw_growth <- growth_rate * sw_ratio

  # Total potential growth
  total_growth <- growth_rate

  # If max_harvest is TRUE, reduce total volume to min_stocking
  if (max_harvest && total_volume > min_stocking) {
    # Calculate total AAC to reduce volume to min_stocking
    total_aac <- total_volume + total_growth - min_stocking
    hw_aac <- total_aac * hw_ratio
    sw_aac <- total_aac * sw_ratio
  } else if (total_volume < min_stocking) {
    # No AAC applied, just growth
    hw_aac <- 0
    sw_aac <- 0
  } else if (total_growth < .25 && min_aac == TRUE) {
    # No AAC applied, just growth
    hw_aac <- .5 * hw_ratio
    sw_aac <- .5 * sw_ratio
  } else {
    # Apply AAC based on the aac_percentage, but check if this cut brings the total volume below minimum
    proposed_hw_aac <- (aac_percentage * total_growth) * hw_ratio
    proposed_sw_aac <- (aac_percentage * total_growth) * sw_ratio

    # Check if the proposed cut brings the total volume below the minimum stocking
    if ((total_volume - (proposed_hw_aac + proposed_sw_aac)) >= min_stocking) {
      # We can cut at the desired AAC percentage
      hw_aac <- proposed_hw_aac
      sw_aac <- proposed_sw_aac
    } else {
      # Cut only the growth, since cutting at the full AAC would drop below minimum
      hw_aac <- total_growth * hw_ratio  # 100% of the growth
      sw_aac <- total_growth * sw_ratio  # 100% of the growth
    }
  }

  return(list(hw_aac = hw_aac, sw_aac = sw_aac,
              growth_rate = total_growth,
              hw_growth = prorated_hw_growth, sw_growth = prorated_sw_growth))
}
