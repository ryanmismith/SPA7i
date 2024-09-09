#' Calculate Growth Rate
#'
#' @description This function calculates the non-linear declining growth rate as the volume approaches its maximum.
#' If the current volume is equal to or greater than the maximum volume, the minimum growth rate is returned.
#' Otherwise, it uses a quadratic decline formula to interpolate the growth rate based on the current and maximum volumes.
#'
#' @details
#' The function implements a non-linear interpolation model to determine the growth rate. As the current volume
#' increases towards the maximum volume, the growth rate declines quadratically. The growth rate never goes below
#' the minimum rate specified.
#'
#' The calculation uses a quadratic interpolation:
#' \deqn{rate = start\_rate - (start\_rate - min\_rate) \times (decline\_factor^{2.2})}
#' where \deqn{decline\_factor = (current\_volume - 14) / (max\_volume - 14)}.
#'
#' The function ensures that the growth rate does not fall below the specified `min_rate`.
#'
#' @param current_volume Numeric, the current volume of the stand (e.g., cords/acre).
#' @param max_volume Numeric, the maximum allowable volume for the stand.
#' @param start_rate Numeric, the initial growth rate when the current volume is below 14 cords.
#' @param min_rate Numeric, the minimum growth rate when the current volume reaches or exceeds the maximum.
#'
#' @family Growth and Yield Functions
#'
#' @return Numeric, the calculated growth rate based on the current volume and maximum volume.
#' The result is constrained between the `min_rate` and `start_rate`.
#'
#' @seealso \code{\link{calculate_max_volume}}
#'
#' @examples
#' # Example 1: Current volume is less than max volume
#' calculate_growth_rate(20, 50, 0.05, 0.01)
#'
#' # Example 2: Current volume is greater than max volume
#' calculate_growth_rate(55, 50, 0.05, 0.01)
#'
#' @export



# Function to calculate the non-linear declining growth rate
calculate_growth_rate <- function(current_volume, max_volume, start_rate, min_rate) {
  if (current_volume >= max_volume) {
    return(min_rate)
  } else {
    # Non-linear interpolation: quadratic decline example
    decline_factor <- (current_volume - 14) / (max_volume - 14)
    rate <- start_rate - (start_rate - min_rate) * (decline_factor^2.2)
    return(max(rate, min_rate))
  }
}
