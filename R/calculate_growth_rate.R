#' Calculate Growth Rate by Township
#'
#' @description
#' `calculate_growth_rate()` computes a non‐linear, power‐law declining annual growth
#' rate for a stand, using its township code to select case‐specific parameters.
#' The curve declines from a user‐specified base volume up to a biological maximum.
#'
#' @details
#' Internally, each township is mapped to one of four “cases” (SP, RY, AE, AW),
#' each with its own softwood start rate, hardwood start rate, exponent \(p\)
#' and minimum floor rate.  The function:
#' 1. Splits total volume into HW and SW components.
#' 2. Computes the biological max via \code{\link{calculate_max_volume}}.
#' 3. Normalizes a “decline factor” from \code{base_vol} to \code{maxvol}.
#' 4. Applies a power‐law:
#'   \deqn{rate = start\_rate - (start\_rate - min\_rate) \times decline\_factor^p,}
#'   enforcing \(\mathrm{rate} \ge \mathrm{min\_rate}\).
#'
#' @param township  Character.  The stand's township code (e.g. "T10R9", "Davis").
#' @param hw_volume Numeric.  Hardwood volume in cords/acre.
#' @param sw_volume Numeric.  Softwood volume in cords/acre.
#' @param base_vol   Numeric.  Volume (cords/acre) below which no decline applies. Default: 6.
#' @param maxvol     Numeric.  Biological maximum volume for decline calculation. Default: 38.
#'
#' @return Numeric.  The predicted annual growth rate (cords/acre/yr), constrained between
#' the case‐specific minimum floor and the weighted start rate.
#'
#' @family Growth and Yield Functions
#' @seealso \code{\link{calculate_max_volume}}, \code{\link{assign_spa_unit}}
#'
#' @examples
#' # pure softwood in RY township:
#' calculate_growth_rate("Davis", hw_volume =  5, sw_volume = 25)
#'
#' # mixed stand in SP township, below base_vol (no decline):
#' calculate_growth_rate("T10R15", hw_volume =  1, sw_volume =  4)
#'
#' # at or above maxvol → floor applies:
#' calculate_growth_rate("Upton", hw_volume = 30, sw_volume = 10)
#'
#' @export

calculate_growth_rate <- function(township = 'T13R5',
                                  hw_volume,
                                  sw_volume,
                                  base_vol = 6,
                                  maxvol   = 38) {

  # parameter lookup table
  param_tbl <- tibble::tribble(
    ~case, ~sw_start, ~hw_start, ~p,    ~min_rate,
    "SP",   0.55,      0.48,      1.85,  0.25,
    "RY",   0.62,      0.54,      1.45,  0.25,
    "AE",   0.57,      0.51,      1.60,  0.25,
    "AW",   0.57,      0.51,      1.60,  0.25
  )

  # map township → case
  unit <- assign_spa_unit(township)
  if (is.na(unit)) {
   unit <- 'AE'
  }

  # extract parameters
  params   <- param_tbl[param_tbl$case == unit, ]
  sw_start <- params$sw_start
  hw_start <- params$hw_start
  p_exp    <- params$p
  min_rate <- params$min_rate

  # total volume safeguard
  total_vol <- hw_volume + sw_volume
  if (total_vol <= 0) {
    hw_volume <- sw_volume <- 0.01
    total_vol <- 0.02
  }

  # biological max
  max_volume <- calculate_max_volume(hw_volume, sw_volume, maxvol = maxvol)

  # if at/above max, return floor
  if (total_vol >= max_volume) {
    return(min_rate)
  }

  # decline fraction
  decline_fac <- pmax(0, (total_vol - base_vol)/(max_volume - base_vol))

  # weighted start rate
  frac_sw    <- sw_volume / total_vol
  frac_hw    <- hw_volume / total_vol
  start_rate <- sw_start*frac_sw + hw_start*frac_hw

  # power‐law decline
  rate <- start_rate - (start_rate - min_rate)*decline_fac^p_exp

  # enforce floor
  max(rate, min_rate)
}

