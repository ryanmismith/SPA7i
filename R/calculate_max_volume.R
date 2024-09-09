#' Calculate Maximum Volume
#'
#' @description This function calculates the maximum standing volume.
#' When the maximum standing volume is reached the
#' SPA growth model defaults to a minimum growth rate.
#'
#' @details
#' The maximum standing volume is a function of the percentage of total
#' volume that is hardwood. This value is used as an input for calculating
#' the growth rate using the \code{calculate_growth_rate} function.
#'
#' The biological maximum using defaults:
#' \itemize{
#'   \item 100\% HW - 38.0 cords/acre
#'   \item 75\% HW  - 39.9 cords/acre
#'   \item 50\% HW  - 41.8 cords/acre
#'   \item 25\% HW  - 43.7 cords/acre
#'   \item 0\% HW   - 45.6 cords/acre
#' }
#'
#' While it is not required that the input volumes be in cords/acre, they
#' must be in the same units. The output will always be cords/acre.
#'
#' The biological maximum is calculated as:
#' \deqn{maxvol \times (1 + (1 - ratio\_hw) \times 0.2)}
#'
#' @param hw_volume Numeric, hardwood volume.
#' @param sw_volume Numeric, softwood volume.
#' @param maxvol Numeric, Defaults to a biological maximum volume of 38 cords/acre.
#'
#' @family Growth and Yield Functions
#'
#' @seealso [SPA7i::calculate_growth_rate]
#'
#' @return Numeric, the maximum standing volume in cords/acre.
#'
#' @author Ryan Smith
#'
#' @examples
#'
#' hw_volume1 <- 18
#' sw_volume1 <- 22
#' calculate_max_volume(hw_volume1, sw_volume1)
#'
#' # Collection of stands:
#'
#' volumes_df <- data.frame(
#'   hw_volume = c(10, 20, 15, 0, 20, 0, 11),
#'   sw_volume = c(5, 10, 8, 2, 0, 20, 11)
#' )
#'
#' volumes_df$max_vol <- apply(volumes_df, 1, function(row) {
#'   calculate_max_volume(row['hw_volume'], row['sw_volume'])
#' })
#'
#' # Display Results
#' print(volumes_df)
#'
#' volumes_df2 <- data.frame(
#'   hw_volume = c(10, 20, 15, 0, 20, 0, 11),
#'   sw_volume = c(5, 10, 8, 2, 0, 20, 11)
#' )
#'
#' volumes_df2$max_vol <- apply(volumes_df2, 1, function(row) {
#'   calculate_max_volume(row['hw_volume'], row['sw_volume'], maxvol = 29)
#' })
#'
#' # Display Results
#' print(volumes_df2)
#'
#' @export



# Function to calculate the maximum volume based on HW and SW ratios
calculate_max_volume <- function(hw_volume, sw_volume, maxvol = 38) {
  total_vol <- hw_volume + sw_volume
  if (total_vol == 0) {
    return(maxvol)  # Prevent division by zero if both volumes are zero
  }
  ratio_hw <- hw_volume / total_vol
  return(round(maxvol * (1 + (1 - ratio_hw) * 0.2), digits = 2))  # Adjust to favor SW dominance
}
