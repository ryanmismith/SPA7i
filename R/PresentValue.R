#' Present Value (PV) Calculation
#'
#' This function calculates the present value of a single cash flow, asset, or expense at a specific point in time using a given discount rate.
#' This is useful for determining the current worth of a future cash flow or value, whether it's income or expense.
#'
#' Use this function with multiple discount rates for sensitivity analysis.
#'
#' @param Flow Numeric. The cash flow, asset value, or expense for which you want to calculate the present value. Positive values represent income or assets, and negative values represent expenses or liabilities.
#' @param Discount Numeric. The discount rate (entered as a decimal, e.g., 0.05 for 5\%). Only a single value is accepted.
#' @param Year Numeric. The number of years from the present (year 0) when the cash flow or value occurs.
#'
#' @return Numeric. The present value of the cash flow, asset, or expense at the specified year and discount rate.
#'
#' @family Value Functions
#'
#' @examples
#' presentValue(100000, 0.06, 30)
#' presentValue(100000, 0.06, 20)
#' presentValue(100000, 0.07, 30)
#' presentValue(100000, 0.07, 20)
#'
#' @export
presentValue <- function(Flow, Year, Discount) {
  PV <- Flow / (1 + Discount)^Year
  return(PV)
}
