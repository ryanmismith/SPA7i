#' Calculate Annual Allowable Cut (AAC) with Prorated Growth Rates
#'
#' @description This function calculates the Annual Allowable Cut (AAC) for hardwood and softwood volumes
#' based on their prorated growth rates and total volume.
#'
#' @details
#' The function calculates the AAC based on the current hardwood (HW) and softwood (SW) volumes, and the
#' percentage of growth to be harvested annually. It calls the \code{calculate_max_volume} function internally
#' to determine the biological maximum standing volume, and the \code{calculate_growth_rate} function to
#' determine the non-linear declining growth rate for HW and SW.
#'
#' The growth rates are prorated based on the ratio of HW and SW volumes relative to the total volume to ensure
#' that each component is appropriately weighted in the AAC calculation.
#'
#' AAC is only calculated if the total volume exceeds a specified minimum threshold (`min_volume_for_aac`).
#'
#' **Note**: The input volumes must be in **cords** for the AAC calculation to be valid.
#'
#' @family Growth and Yield Functions
#'
#' @seealso \code{\link{calculate_max_volume}}
#' @seealso \code{\link{calculate_growth_rate}}
#'
#' @examples
#' # Example 1: Simple calculation of AAC for one year
#' hw_volume <- 20
#' sw_volume <- 15
#' aac_percentage <- 0.5
#' min_volume_for_aac <- 30
#' calculate_aac(hw_volume, sw_volume, aac_percentage, min_volume_for_aac)
#'
#' # Example 2: Simulate AAC over 20 years
#' hw_volume <- 14  # Half of total volume (28)
#' sw_volume <- 14  # Half of total volume (28)
#' aac_percentage <- 0.5  # 50% of the growth can be harvested
#' min_volume_for_aac <- 28.5  # Minimum volume before AAC calculation
#' years <- 20  # Number of years to run the loop
#'
#' # Data storage
#' results <- data.frame(Year = numeric(), HW_Volume = numeric(), SW_Volume = numeric(), HW_AAC = numeric(), SW_AAC = numeric())
#'
#' # Loop over 20 years
#' for (year in 1:years) {
#'   # Calculate AAC for the year
#'   aac_result <- calculate_aac(hw_volume, sw_volume, aac_percentage, min_volume_for_aac)
#'
#'   # Update volumes for next year
#'   hw_volume <- hw_volume + aac_result$hw_growth - aac_result$hw_aac
#'   sw_volume <- sw_volume + aac_result$sw_growth - aac_result$sw_aac
#'   total_volume <- hw_volume + sw_volume
#'
#'   # Store the results
#'   results <- rbind(results, data.frame(
#'     Year = year,
#'     HW_Volume = hw_volume,
#'     SW_Volume = sw_volume,
#'     Total_Volume = total_volume,
#'     HW_AAC = aac_result$hw_aac,
#'     SW_AAC = aac_result$sw_aac
#'   ))
#' }
#'
#' # Display the results
#' print(results)
#'
#' @export

# calculate_aac <- function(hw_volume, sw_volume, aac_percentage, min_volume_for_aac) {
#   total_volume <- hw_volume + sw_volume
#
#   if (total_volume == 0) {
#     return(list(hw_aac = 0, sw_aac = 0, hw_growth = 0, sw_growth = 0))
#   }
#
#   # Calculate the maximum volume using calculate_max_volume
#   max_volume <- calculate_max_volume(hw_volume, sw_volume)
#
#   # Calculate HW and SW growth rates using calculate_growth_rate function
#   hw_growth_rate <- calculate_growth_rate(hw_volume, max_volume, start_rate = 0.05, min_rate = 0.01)
#   sw_growth_rate <- calculate_growth_rate(sw_volume, max_volume, start_rate = 0.06, min_rate = 0.02)
#
#   # Calculate the ratio of HW and SW based on the total volume
#   hw_ratio <- if (hw_volume == 0) { 0 } else { hw_volume / total_volume }
#   sw_ratio <- if (sw_volume == 0) { 0 } else { sw_volume / total_volume }
#
#   # Prorate the growth rates by their respective volume ratios
#   prorated_hw_growth <- hw_growth_rate * hw_ratio
#   prorated_sw_growth <- sw_growth_rate * sw_ratio
#
#   # Check if total volume is below the minimum volume required for AAC
#   if (total_volume < min_volume_for_aac) {
#     # No AAC when total volume is below the threshold
#     hw_aac <- 0
#     sw_aac <- 0
#   } else {
#     # Calculate AAC as a percentage of the prorated growth
#     hw_aac <- aac_percentage * prorated_hw_growth
#     sw_aac <- aac_percentage * prorated_sw_growth
#   }
#
#   # Ensure we are not cutting more than the total available volume
#   hw_aac <- min(hw_aac, hw_volume)
#   sw_aac <- min(sw_aac, sw_volume)
#
#   return(list(hw_aac = hw_aac, sw_aac = sw_aac, hw_growth = prorated_hw_growth, sw_growth = prorated_sw_growth))
# }
#
#
