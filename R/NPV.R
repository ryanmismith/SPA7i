#' Net Present Value (NPV) Calculation
#'
#' This function calculates the Net Present Value (NPV) of a series of cash flows (positive or negative)
#' at a given discount rate. It automatically adjusts flows classified as "Expense" or "Negative" to
#' negative values if they are not already negative, and handles flows classified as "Revenue" or "Positive"
#' by ensuring they are positive. If no class is provided, it assumes that negative values are expenses and positive values are revenues.
#'
#' @param Flow Numeric vector. A series of cash flow values, where positive values represent revenue and negative values represent expenses.
#' @param Occurrence Numeric vector. The years from today when each cash flow occurs. Year 0 represents the present.
#' @param Discount Numeric. The discount rate to be applied to each cash flow, entered as a decimal (e.g., 0.05 for 5\%).
#' @param Class Optional character vector. Classify each flow as "Expense" or "Revenue", or "Negative" or "Positive". If missing, the function will assume that positive flows are revenue and negative flows are expenses.
#'
#' @return Numeric. The Net Present Value (NPV) of the cash flows based on the given discount rate.
#'
#' @details
#' The function computes the present value of each cash flow, which may represent either revenue or expense.
#' Cash flows classified as "Expense" or "Negative" are automatically converted to negative values if they are not already negative, and cash flows classified as "Revenue" or "Positive" are ensured to be positive.
#' If no `Class` vector is provided, the function assumes that positive flows are revenue and negative flows are expenses.
#'
#' @family Value Functions
#'
#' @examples
#' Flow <- c(-400, -100, 60, 200, 6600)
#' Occurrence <- c(0, 5, 8, 15, 30)
#' Discount <- 0.06
#' simpleNPV(Flow, Occurrence, Discount)
#'
#' \dontrun{
#' # Example with class vector
#' Flow <- c(400, 100, 60, 200, 6600)
#' Class <- c("Expense", "Expense", "Revenue", "Revenue", "Revenue")
#' Occurrence <- c(0, 5, 8, 15, 30)
#' Discount <- 0.06
#' simpleNPV(Flow, Occurrence, Discount, Class = Class)
#' }
#' @export

simpleNPV <- function(Flow, Occurrence, Discount, Class = NULL) {

  # Adjust flows based on class if provided
  if (!is.null(Class)) {
    Flow <- ifelse(tolower(Class) %in% c("expense", "negative") & Flow > 0, -abs(Flow), Flow)
    Flow <- ifelse(tolower(Class) %in% c("revenue", "positive") & Flow < 0, abs(Flow), Flow)
  }

  # If no class is provided, assume negative values are expenses and positive values are revenues
  Flow <- ifelse(Flow > 0, Flow, -abs(Flow))  # Ensure negative values are treated as expenses

  # Vectorized calculation for Present Value: PV = Flow / (1 + Discount)^Occurrence
  PresentValues <- Flow / (1 + Discount)^Occurrence

  # Sum of Present Values gives NPV
  NPV <- round(sum(PresentValues), 2)

  return(NPV)
}
