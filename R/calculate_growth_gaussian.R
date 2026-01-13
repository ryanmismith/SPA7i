#' Calculate Growth Rate by Township (Gaussian-Based)
#'
#' @description
#' `calculate_growth_rate()` estimates annual stand-level growth using a
#' species-specific Gaussian model. Each township is mapped to a case unit
#' (SP, RY, AE, AW), which determines the hardwood and softwood growth curve parameters.
#'
#' @details
#' Growth is calculated independently for hardwood and softwood using the function:
#'
#' \deqn{G(V) = a \cdot \exp\left(-\frac{1}{2} \left(\frac{V - \mu}{\sigma}\right)^2\right)}
#'
#' where:
#'
#' - \( G(V) \) is the growth at volume \( V \),
#' - \( a \) is the peak growth rate (tempered from original fits),
#' - \( \mu \) is the volume at which peak growth occurs, and
#' - \( \sigma \) controls the spread of the growth curve.
#'
#' The final growth rate is the weighted average of hardwood and softwood growth,
#' based on their proportion of total volume:
#'
#' \deqn{
#' \text{growth}_\text{total} =
#' \frac{V_{\mathrm{HW}}}{V} \cdot G_{\mathrm{HW}} +
#' \frac{V_{\mathrm{SW}}}{V} \cdot G_{\mathrm{SW}}
#' }
#'
#' Parameters are derived from nonlinear least squares fits to empirical data
#' and then scaled (a × 0.9) to temper model optimism based on field observations.
#'
#' If total volume is zero or negative, the function returns 0.
#'
#' @param township Character. The stand's township code (e.g., `"T13R5"`, `"Davis"`).
#' Used to assign the growth curve parameters via \code{\link{assign_spa_unit}}.
#' @param hw_volume Numeric. Hardwood volume in cords/acre.
#' @param sw_volume Numeric. Softwood volume in cords/acre.
#'
#' @return Numeric. Predicted annual growth rate (cords/acre/year), calculated
#' as the weighted mean of hardwood and softwood Gaussian growth curves.
#'
#' @family Growth and Yield Functions
#' @seealso \code{\link{assign_spa_unit}}, \code{\link{calculate_growth_rate}}
#'
#' @examples
#' # Pure softwood stand in high-productivity township
#' calculate_growth_rate("T13R5", hw_volume = 0, sw_volume = 30)
#'
#' # Mixed stand with moderate volumes
#' calculate_growth_rate("Davis", hw_volume = 10, sw_volume = 10)
#'
#' # Edge case: no volume
#' calculate_growth_rate("T15R13", hw_volume = 0, sw_volume = 0)
#'
#' @export

calculate_growth_rate <- function(township = 'T13R5',
                                  hw_volume,
                                  sw_volume,
                                  base_vol = 6,
                                  maxvol = 38) {
  maxvol = maxvol
  base_vol = base_vol
  # Parameters per unit and species group
  param_tbl <- tibble::tribble(
    ~case, ~hw_a, ~hw_mu, ~hw_sigma, ~sw_a, ~sw_mu, ~sw_sigma,
    "SP",  0.483,  32.0,    22.6,      0.522,  26.5,    20.4,
    "RY",  0.479,  28.3,    24.5,      0.558,  29.4,    21.5,
    "AE",  0.481,  30.2,    23.6,      0.540,  28.0,    20.9,
    "AW",  0.481,  30.2,    23.6,      0.540,  28.0,    20.9
  )

  # map township → case
  unit <- assign_spa_unit(township)
  if (is.na(unit)) {
    # Normalize the township (uppercase, trim spaces)
    tname <- toupper(trimws(as.character(township)))

    # If township itself is one of the valid district codes (RY, SP, AW, AE)
    if (tname %in% c("RY", "SP", "AW", "AE")) {
      unit <- tname
    } else {
      unit <- "AE"
    }
  }

  # Extract parameters
  params <- param_tbl[param_tbl$case == unit, ]

  # Total volume safeguard
  total_vol <- hw_volume + sw_volume
  if (total_vol <= 0) {
    return(0)
  }

  # Gaussian growth function
  gauss <- function(vol, a, mu, sigma) {
    a * exp(-0.5 * ((vol - mu) / sigma)^2)
  }

  # Weighted mean growth rate
  frac_hw <- hw_volume / total_vol
  frac_sw <- sw_volume / total_vol

  gh_tot <- gauss(total_vol, params$hw_a, params$hw_mu, params$hw_sigma)
  gs_tot <- gauss(total_vol, params$sw_a, params$sw_mu, params$sw_sigma)

  growth_total <- frac_hw * gh_tot + frac_sw * gs_tot

  return(growth_total)
}

