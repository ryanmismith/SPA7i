#' Tornado Sensitivity for Deterministic NPV
#'
#' Computes a one-way tornado sensitivity by perturbing selected drivers
#' (flows by driver label, the discount rate, and/or terminal value) to low/high cases
#' while holding all other inputs fixed. Returns a sorted table of low/high
#' deltas versus baseline NPV and a ggplot object for the tornado chart.
#'
#' Deterministic NPV = PV(annual flows) + terminal component at TerminalYear.
#' If Exit = TRUE, terminal PV uses presentValue(FutureValue, TerminalYear, NominalRate);
#' else landExpectVal(FutureValue, TerminalYear, NominalRate).
#'
#' @param Flow numeric, cash flows.
#' @param Occurrence numeric, periods (1..T), same length as Flow.
#' @param Name character, flow labels, same length as Flow.
#' @param NominalRate numeric scalar discount rate (e.g., 0.05).
#' @param TerminalYear integer, terminal year for terminal value.
#' @param FutureValue numeric scalar, terminal value at TerminalYear (undiscounted).
#' @param Exit logical; TRUE uses present value terminal (exit/sale), FALSE uses land expectation value at TerminalYear.
#' @param ShockSpec data.frame with columns: driver, type in {"flow","flow_name","rate","terminal"}, low_mult, high_mult.
#' @param Driver optional character vector (same length as Flow) giving the driver label for each row in the flow table.
#'               If NULL, the function falls back to FlowNameMap or Name.
#' @param FlowNameMap optional named list mapping values in Name -> driver labels (ignored if Driver is provided).
#'
#' @details
#' \strong{Driver matching.} Each flow row can carry a \code{Driver} label. Shocks of
#' \code{type="flow"} are applied to all rows where \code{flows_df$Driver == driver}.
#' If \code{Driver} is not supplied, \code{FlowNameMap} (a named list mapping \code{Name}
#' -> driver label) is used; if that is also \code{NULL}, the \code{Name} itself is used
#' as the driver label.
#'
#' \strong{ShockSpec.} A data frame with columns:
#' \itemize{
#'   \item \code{driver}: character label to match against \code{Driver} (or mapped \code{Name}).
#'   \item \code{type}: one of \code{"flow"}, \code{"rate"}, \code{"terminal"}.
#'         (\code{"flow_name"} is accepted as an alias for \code{"flow"} for backward compatibility.)
#'   \item \code{low_mult}, \code{high_mult}: numeric multipliers for the low/high cases.
#' }
#'
#' \strong{How multipliers are applied.}
#' \itemize{
#'   \item \code{type="flow"}: multiply \code{Value} for matching rows by the multiplier.
#'   \item \code{type="rate"}: multiply \code{NominalRate} by the multiplier
#'         (e.g., \code{1.15} = +15\% relative change in the rate).
#'   \item \code{type="terminal"}: multiply \code{FutureValue} by the multiplier.
#' }
#'
#' Low/high multipliers should reflect the intended directionality. For costs, a common
#' convention is \code{low_mult > 1} and \code{high_mult < 1}; for revenues, the opposite.
#'
#' \strong{ShockSpec example.}
#' \preformatted{
#' data.frame(
#'   driver    = c("Discount Rate","Harvest Revenue","Operating Costs","Terminal Value"),
#'   type      = c("rate",         "flow",           "flow",           "terminal"),
#'   low_mult  = c(1.15,           0.85,             1.15,             0.85),
#'   high_mult = c(0.85,           1.15,             0.85,             1.15)
#' )
#' }
#'
#' \strong{NPV convention.} Deterministic NPV = present value of annual flows plus a
#' terminal component at \code{TerminalYear}. If \code{Exit=TRUE}, the terminal is
#' discounted with \code{presentValue(FutureValue, TerminalYear, NominalRate)}; otherwise,
#' \code{landExpectVal(FutureValue, TerminalYear, NominalRate)} is used.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{baseline_npv} Numeric scalar, the baseline NPV under the input assumptions.
#'   \item \code{tornado_table} \code{data.frame} with columns:
#'         \code{driver}, \code{low_delta}, \code{high_delta}, \code{range}.
#'         Deltas are low/high NPV minus baseline NPV (same units as NPV).
#'         The table is sorted in \emph{descending} \code{range}, where
#'         \code{range = |low_delta| + |high_delta|}.
#'   \item \code{plot} A \code{ggplot} tornado chart built from \code{tornado_table}.
#'   \item \code{assumptions} A list echoing key inputs and metadata, including
#'         \code{NominalRate}, \code{TerminalYear}, \code{FutureValue}, \code{Exit},
#'         the \code{ShockSpec} used, any \code{FlowNameMap} provided, and the
#'         driver source (\code{"Driver"}, \code{"FlowNameMap"}, or \code{"Name"}).
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_vline labs theme_minimal
#' @importFrom dplyr %>% arrange bind_rows mutate group_by summarise ungroup select
#' @examples
#' \dontrun{
#' # Minimal helpers for terminal handling used by tornadoSensitivity():
#' presentValue  <- function(fv, n, r) fv / (1 + r)^n
#' landExpectVal <- function(a, n, r) (a / r) / (1 + r)^n
#'
#' # Toy cash-flow schedule (revenues positive, costs negative)
#' flows <- data.frame(
#'   Year   = c(1, 1, 5, 5, 10, 10, 15, 15),
#'   Name   = c("Harvest Revenue","Operating Expense",
#'              "Harvest Revenue","Operating Expense",
#'              "Harvest Revenue","Operating Expense",
#'              "Harvest Revenue","Operating Expense"),
#'   Driver = c("Harvest Revenue","Operating Costs",
#'              "Harvest Revenue","Operating Costs",
#'              "Harvest Revenue","Operating Costs",
#'              "Harvest Revenue","Operating Costs"),
#'   Value  = c(50000, -15000,
#'              65000, -17000,
#'              80000, -18000,
#'              90000, -20000),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Shock specification: rate, flows by Driver label, and terminal value
#' ShockSpec <- data.frame(
#'   driver    = c("Discount Rate", "Harvest Revenue", "Operating Costs", "Terminal Value"),
#'   type      = c("rate",          "flow",            "flow",            "terminal"),
#'   low_mult  = c(1.15,            0.85,              1.15,              0.85),
#'   high_mult = c(0.85,            1.15,              0.85,              1.15),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Parameters
#' r  <- 0.06
#' Tm <- 20
#' FV <- 250000  # if Exit=TRUE: lump-sum at year Tm; if Exit=FALSE: annualized steady-state amount
#'
#' # Run with explicit Driver column (preferred)
#' out <- tornadoSensitivity(
#'   Flow         = flows$Value,
#'   Occurrence   = flows$Year,
#'   Name         = flows$Name,
#'   Driver       = flows$Driver,
#'   NominalRate  = r,
#'   TerminalYear = Tm,
#'   FutureValue  = FV,
#'   Exit         = TRUE,      # set FALSE to use land expectation value at Tm
#'   ShockSpec    = ShockSpec
#' )
#'
#' # Inspect results
#' round(out$baseline_npv, 2)
#' transform(out$tornado_table,
#'           low_delta  = round(low_delta,  2),
#'           high_delta = round(high_delta, 2),
#'           range      = round(range,      2))
#'
#' # Plot
#' out$plot
#'
#' # Variation: if you do NOT carry a Driver column,
#' # you can map Name -> driver labels with FlowNameMap:
#' FlowNameMap <- list(
#'   "Harvest Revenue"   = "Harvest Revenue",
#'   "Operating Expense" = "Operating Costs"
#' )
#' out2 <- tornadoSensitivity(
#'   Flow         = flows$Value,
#'   Occurrence   = flows$Year,
#'   Name         = flows$Name,
#'   NominalRate  = r,
#'   TerminalYear = Tm,
#'   FutureValue  = FV,
#'   Exit         = FALSE,     # use LEV at terminal year
#'   ShockSpec    = ShockSpec,
#'   FlowNameMap  = FlowNameMap
#' )
#' round(out2$baseline_npv, 2)
#' out2$plot
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
    Driver = NULL,
    FlowNameMap = NULL
) {
  # ---- Input checks
  stopifnot(
    length(Flow) == length(Occurrence),
    length(Flow) == length(Name),
    is.numeric(NominalRate), length(NominalRate) == 1L,
    length(TerminalYear) == 1L,
    length(FutureValue) == 1L,
    is.data.frame(ShockSpec)
  )
  req_cols <- c("driver","type","low_mult","high_mult")
  if (!all(req_cols %in% names(ShockSpec))) {
    stop("ShockSpec must have columns: ", paste(req_cols, collapse = ", "))
  }

  # ---- Build flows table with a Driver column
  if (!is.null(Driver)) {
    if (length(Driver) != length(Flow)) stop("Driver must be same length as Flow.")
    drv <- as.character(Driver)
  } else if (!is.null(FlowNameMap)) {
    # Map Name -> driver; default to Name if not found in map
    nm <- as.character(Name)
    drv <- vapply(nm, function(n) {
      v <- FlowNameMap[[n]]
      if (is.null(v)) n else as.character(v)
    }, FUN.VALUE = character(1))
  } else {
    drv <- as.character(Name)
  }

  flows_df <- data.frame(
    Driver = drv,
    Name   = as.character(Name),
    Year   = as.numeric(Occurrence),
    Value  = as.numeric(Flow),
    stringsAsFactors = FALSE
  )

  # ---- Deterministic NPV under baseline
  pv_flows <- function(df, rate) {
    # assumes Year is 1..T (or any positive periods)
    sum(df$Value / (1 + rate) ^ df$Year, na.rm = TRUE)
  }
  pv_term <- function(fv, rate) {
    if (Exit) {
      presentValue(fv, TerminalYear, rate)
    } else {
      landExpectVal(fv, TerminalYear, rate)
    }
  }
  baseline_npv <- pv_flows(flows_df, NominalRate) + pv_term(FutureValue, NominalRate)

  # ---- Apply a single driver perturbation and compute deltas
  valid_types <- c("flow","flow_name","rate","terminal")
  ShockSpec$type <- as.character(ShockSpec$type)
  if (!all(ShockSpec$type %in% valid_types)) {
    stop("ShockSpec$type must be one of: ", paste(valid_types, collapse = ", "))
  }

  apply_driver <- function(driver, type, low_mult, high_mult) {
    # LOW
    flows_low <- flows_df
    rate_low  <- NominalRate
    fv_low    <- FutureValue

    if (type %in% c("flow","flow_name")) {
      idx <- flows_low$Driver == driver
      if (!any(idx)) warning(sprintf("Driver '%s' not found in flows_df$Driver.", driver))
      flows_low$Value[idx] <- flows_low$Value[idx] * low_mult
    } else if (type == "rate") {
      rate_low <- NominalRate * low_mult
    } else if (type == "terminal") {
      fv_low <- FutureValue * low_mult
    }
    npv_low <- pv_flows(flows_low, rate_low) + pv_term(fv_low, rate_low)

    # HIGH
    flows_high <- flows_df
    rate_high  <- NominalRate
    fv_high    <- FutureValue

    if (type %in% c("flow","flow_name")) {
      idx <- flows_high$Driver == driver
      flows_high$Value[idx] <- flows_high$Value[idx] * high_mult
    } else if (type == "rate") {
      rate_high <- NominalRate * high_mult
    } else if (type == "terminal") {
      fv_high <- FutureValue * high_mult
    }
    npv_high <- pv_flows(flows_high, rate_high) + pv_term(fv_high, rate_high)

    data.frame(
      driver     = as.character(driver),
      low_delta  = npv_low  - baseline_npv,
      high_delta = npv_high - baseline_npv,
      stringsAsFactors = FALSE
    )
  }

  parts <- mapply(
    apply_driver,
    driver    = ShockSpec$driver,
    type      = ShockSpec$type,
    low_mult  = ShockSpec$low_mult,
    high_mult = ShockSpec$high_mult,
    SIMPLIFY = FALSE
  )
  tornado_table <- do.call(rbind, parts)

  # Range measure for sorting (typical tornado: largest at top)
  tornado_table$range <- abs(tornado_table$low_delta) + abs(tornado_table$high_delta)
  tornado_table <- tornado_table[order(-tornado_table$range), , drop = FALSE]

  # ---- Plot
  # Build a min/max per driver so bars always straddle toward 0 correctly
  plot_df <- transform(
    tornado_table,
    y = factor(driver, levels = rev(unique(tornado_table$driver))),
    xmin = pmin(low_delta, high_delta),
    xmax = pmax(low_delta, high_delta)
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = y)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = as.numeric(y) - 0.4, ymax = as.numeric(y) + 0.4),
      alpha = 0.8
    ) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::labs(
      title = "NPV Tornado – One-way Sensitivity",
      x = expression(Delta~NPV~"(vs. baseline)"),
      y = NULL
    ) +
    ggplot2::theme_minimal()

  list(
    baseline_npv  = baseline_npv,
    tornado_table = tornado_table,
    plot          = p,
    assumptions   = list(
      NominalRate   = NominalRate,
      TerminalYear  = TerminalYear,
      FutureValue   = FutureValue,
      Exit          = Exit,
      ShockSpec     = ShockSpec,
      driver_source = if (!is.null(Driver)) "Driver"
      else if (!is.null(FlowNameMap)) "FlowNameMap"
      else "Name"
    )
  )
}
