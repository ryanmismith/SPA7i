#' Tornado Sensitivity for Deterministic NPV
#'
#' Computes a one-way tornado sensitivity by perturbing selected drivers
#' (flows by name, the discount rate, and/or terminal value) to low/high cases
#' while holding all other inputs fixed. Returns a sorted table of low/high
#' deltas versus baseline NPV and a ggplot object for the tornado chart.
#'
#' Deterministic NPV is computed as the present value of the annual flow stream
#' plus a terminal component at \code{TerminalYear}. If \code{Exit = TRUE},
#' the terminal is discounted with \code{presentValue(FutureValue, TerminalYear, NominalRate)};
#' otherwise, \code{landExpectVal(FutureValue, TerminalYear, NominalRate)} is used.
#'
#' @param Flow Numeric vector of cash flows (length = n rows).
#' @param Occurrence Numeric vector of periods (1..T) for each cash flow; same length as \code{Flow}.
#' @param Name Character vector of flow labels (e.g., "Harvest Revenue", "Operating Expense"); same length as \code{Flow}.
#' @param NominalRate Numeric scalar, the discount rate (e.g., 0.05).
#' @param TerminalYear Integer terminal year at which the terminal value is applied.
#' @param FutureValue Numeric scalar, terminal value at \code{TerminalYear} (undiscounted).
#' @param Exit Logical; if \code{TRUE}, use present value for the terminal (exit/sale).
#'   If \code{FALSE}, use \code{landExpectVal} (perpetuity) at the terminal year.
#' @param ShockSpec A \code{data.frame} defining drivers and low/high multipliers with
#'   columns: \code{driver} (label), \code{type} (one of "flow_name","rate","terminal"),
#'   \code{low_mult}, \code{high_mult}. See Details.
#' @param FlowNameMap Optional named list mapping the flow labels present in \code{Name}
#'   to the friendly driver labels used in \code{ShockSpec}. Keys are the exact strings in
#'   \code{Name}, values are the \code{driver} labels. If \code{NULL}, the function assumes
#'   \code{driver} entries in \code{ShockSpec} directly match values of \code{Name}.
#'
#' @details
#' \strong{ShockSpec} examples:
#' \preformatted{
#' data.frame(
#'   driver = c("Discount Rate", "Harvest Revenue", "Operating Costs", "Terminal Value"),
#'   type   = c("rate",          "flow_name",       "flow_name",       "terminal"),
#'   low_mult  = c(1.15,          0.85,              1.15,              0.85),
#'   high_mult = c(0.85,          1.15,              0.85,              1.15)
#' )
#' }
#' Low/high multipliers should reflect the intended directionality
#' (e.g., for costs you typically increase for "low" and decrease for "high").
#'
#' @return A list with:
#' \itemize{
#'   \item \code{baseline_npv} Numeric scalar.
#'   \item \code{tornado_table} \code{data.frame} with columns
#'         \code{driver}, \code{low_delta}, \code{high_delta}, \code{range} (sorted).
#'   \item \code{plot} A ggplot object for the tornado chart.
#'   \item \code{assumptions} A list echoing key inputs and the \code{ShockSpec} used.
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_vline labs theme_minimal
#' @importFrom dplyr %>% arrange bind_rows mutate group_by summarise ungroup select
#' @examples
#' \dontrun{
#' ShockSpec <- data.frame(
#'   driver   = c("Discount Rate","Harvest Revenue","Operating Costs","Terminal Value"),
#'   type     = c("rate","flow_name","flow_name","terminal"),
#'   low_mult = c(1.15, 0.85, 1.15, 0.85),
#'   high_mult= c(0.85, 1.15, 0.85, 1.15)
#' )
#' out <- tornadoSensitivity(
#'   Flow = master_flows$Value,
#'   Occurrence = master_flows$Year,
#'   Name = master_flows$Name,
#'   NominalRate = discount_rate,
#'   TerminalYear = years,
#'   FutureValue = final_standing_value,
#'   Exit = TRUE,
#'   ShockSpec = ShockSpec,
#'   FlowNameMap = list(
#'     "Harvest Revenue"   = "Harvest Revenue",
#'     "Operating Expense" = "Operating Costs"
#'   )
#' )
#' out$plot
#' out$tornado_table
#' }
#' @export
tornadoSensitivity <- function(
    Flow,
    Occurrence,
    Name,
    NominalRate,
    TerminalYear,
    FutureValue,
    Exit = TRUE,
    ShockSpec,
    FlowNameMap = NULL
) {
  # -- Input checks
  stopifnot(length(Flow) == length(Occurrence),
            length(Flow) == length(Name),
            is.numeric(NominalRate),
            length(TerminalYear) == 1L,
            length(FutureValue) == 1L)
  
  flows_df <- data.frame(
    Name = as.character(Name),
    Year = as.numeric(Occurrence),
    Value = as.numeric(Flow),
    stringsAsFactors = FALSE
  )
  
  # deterministic NPV under current assumptions
  pv_flows <- function(df, rate) sum(df$Value / (1 + rate) ^ df$Year, na.rm = TRUE)
  pv_term  <- function(fv, rate) {
    if (Exit) {
      presentValue(fv, TerminalYear, rate)
    } else {
      landExpectVal(fv, TerminalYear, rate)
    }
  }
  baseline_npv <- pv_flows(flows_df, NominalRate) + pv_term(FutureValue, NominalRate)
  
  # helper: map flow names to driver label if FlowNameMap provided
  # returns logical vector rows to tweak for a given driver label
  rows_for_driver <- function(driver_label) {
    if (is.null(FlowNameMap)) {
      flows_df$Name %in% driver_label
    } else {
      # find all flow names whose mapped driver equals driver_label
      keys <- names(FlowNameMap)[vapply(FlowNameMap, function(v) v == driver_label, logical(1))]
      flows_df$Name %in% keys
    }
  }
  
  # apply one driver perturbation (low/high) and compute deltas vs baseline
  apply_driver <- function(driver, type, low_mult, high_mult) {
    # LOW case
    flows_low  <- flows_df
    rate_low   <- NominalRate
    fv_low     <- FutureValue
    
    if (type == "flow_name") {
      idx <- rows_for_driver(driver)
      if (any(idx)) flows_low$Value[idx] <- flows_low$Value[idx] * low_mult
    } else if (type == "rate") {
      rate_low <- NominalRate * low_mult
    } else if (type == "terminal") {
      fv_low <- FutureValue * low_mult
    } else {
      stop("Unknown driver type: ", type)
    }
    npv_low <- pv_flows(flows_low, rate_low) + pv_term(fv_low, rate_low)
    
    # HIGH case
    flows_high <- flows_df
    rate_high  <- NominalRate
    fv_high    <- FutureValue
    
    if (type == "flow_name") {
      idx <- rows_for_driver(driver)
      if (any(idx)) flows_high$Value[idx] <- flows_high$Value[idx] * high_mult
    } else if (type == "rate") {
      rate_high <- NominalRate * high_mult
    } else if (type == "terminal") {
      fv_high <- FutureValue * high_mult
    }
    npv_high <- pv_flows(flows_high, rate_high) + pv_term(fv_high, rate_high)
    
    data.frame(
      driver = driver,
      low_delta  = npv_low  - baseline_npv,
      high_delta = npv_high - baseline_npv,
      stringsAsFactors = FALSE
    )
  }
  
  # compute deltas for all drivers
  parts <- mapply(
    apply_driver,
    driver    = ShockSpec$driver,
    type      = ShockSpec$type,
    low_mult  = ShockSpec$low_mult,
    high_mult = ShockSpec$high_mult,
    SIMPLIFY = FALSE
  )
  tornado_table <- do.call(rbind, parts)
  tornado_table$range <- abs(tornado_table$low_delta) + abs(tornado_table$high_delta)
  tornado_table <- tornado_table[order(tornado_table$range), ]
  
  # build the plot
  p <- ggplot2::ggplot(tornado_table, ggplot2::aes(y = stats::reorder(driver, range))) +
    ggplot2::geom_bar(ggplot2::aes(x = high_delta), stat = "identity", alpha = 0.8) +
    ggplot2::geom_bar(ggplot2::aes(x = low_delta),  stat = "identity", alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::labs(
      title = "NPV Tornado – One-way Sensitivity",
      x = expression(Delta~NPV~"(relative to baseline)"),
      y = NULL
    ) +
    ggplot2::theme_minimal()
  
  list(
    baseline_npv = baseline_npv,
    tornado_table = tornado_table,
    plot = p,
    assumptions = list(
      NominalRate = NominalRate,
      TerminalYear = TerminalYear,
      FutureValue = FutureValue,
      Exit = Exit,
      ShockSpec = ShockSpec,
      FlowNameMap = FlowNameMap
    )
  )
}
