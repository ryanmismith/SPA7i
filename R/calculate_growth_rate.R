#' Calculate Growth Rate
#'
#' @description This function calculates the non-linear declining growth rate for a stand or matrix based on its hardwood (HW) and softwood (SW) volumes
#' as it approaches its calculated `max_volume` using the \code{\link{calculate_max_volume}} function. It factors in different starting rates for HW and SW growth
#' to address differences in growth behavior between them.
#'
#' @details
#' The function calculates the maximum allowable volume using the \code{\link{calculate_max_volume}} function, which can be influenced by the optional `maxvol` argument (defaulting to 38 cords).
#' It then uses a non-linear interpolation model to determine the growth rate as the combined HW and SW volumes increase towards the calculated maximum.
#' The growth rate declines quadratically and never goes below the specified minimum rate.
#'
#' The calculation uses a quadratic interpolation:
#' \deqn{rate = start\_rate - (start\_rate - min\_rate) \times (decline\_factor^{2})}
#' where \deqn{decline\_factor = (current\_volume - 14) / (max\_volume - 14)}.
#'
#' The starting rate for the growth calculation is determined by the proportion of HW and SW volumes and their respective
#' starting rates, with default values of 0.45 for SW and 0.40 for HW. The final growth rate is constrained between
#' the minimum rate and the calculated start rate.
#'
#' @param hw_volume Numeric, the current volume of hardwood (e.g., cords/acre).
#' @param sw_volume Numeric, the current volume of softwood (e.g., cords/acre).
#' @param sw_max_rate Numeric, the initial growth rate for softwood, default is 0.45.
#' @param hw_max_rate Numeric, the initial growth rate for hardwood, default is 0.40.
#' @param min_rate Numeric, the minimum growth rate when the combined volume reaches or exceeds the maximum, default is 0.02.
#' @param maxvol Numeric, optional argument that defines the maximum allowable volume, default is 38 cords.
#'
#' @family Growth and Yield Functions
#'
#' @return Numeric, the calculated growth rate based on the combined HW and SW volumes and the calculated maximum volume from \code{\link{calculate_max_volume}}.
#' The result is constrained between the `min_rate` and the combined `start_rate`.
#'
#' @seealso \code{\link{calculate_max_volume}}
#'
#' @examples
#' # Example 1: Current volume is less than but approaching max volume
#' hw_volumes <- c(4, 7, 12, 17, 20)
#' sw_volumes <- c(4, 7, 12, 17, 20)
#' sapply(1:length(hw_volumes), function(i) calculate_growth_rate(hw_volumes[i], sw_volumes[i]))
#'
#' # Example 2: Current volume is greater than max volume
#' calculate_growth_rate(24, 22)
#'
#' # Example 3: No Hardwood
#' sw_volumes_no_hw <- c(14, 24, 34, 44)
#' sapply(sw_volumes_no_hw, function(sw) calculate_growth_rate(0, sw))
#'
#' # Example 4: No Softwood
#' hw_volumes_no_sw <- c(14, 24, 34, 44)
#' sapply(hw_volumes_no_sw, function(hw) calculate_growth_rate(hw, 0))
#'
#' @export


# Function to calculate the non-linear declining growth rate
calculate_growth_rate <- function(hw_volume, sw_volume, max_volume, sw_max_rate = .45, hw_max_rate = .40, min_rate = .02, maxvol = 38) {
  percent_sw <- sw_volume / (hw_volume + sw_volume)
  percent_hw <- hw_volume / (hw_volume + sw_volume)
  sw_start_rate <- sw_max_rate * percent_sw
  hw_start_rate <- hw_max_rate * percent_hw
  start_rate <- sw_start_rate + hw_start_rate
  current_volume <- hw_volume + sw_volume
  max_volume <- calculate_max_volume(hw_volume = hw_volume, sw_volume = sw_volume, maxvol = maxvol)

  if (current_volume >= max_volume) {
    return(min_rate)
  } else if (max_volume <= 14) {
    stop("max_volume must be greater than 14 to avoid division by zero")
  } else {
    # Non-linear interpolation: quadratic decline example
    decline_factor <- pmax(0,(current_volume - 14) / (max_volume - 14))
    rate <- start_rate - (start_rate - min_rate) * (decline_factor^2)
    result <- round(max(rate, min_rate),3)
    return(result)
  }
}
