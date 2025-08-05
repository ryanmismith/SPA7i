#' Monte Carlo NPV Simulation with Exit Option for Terminal Year
#'
#' This function performs Monte Carlo simulations to calculate Net Present Value (NPV) under uncertainty.
#' It uses simpleNPV for regular cash flows, and either landExpectVal (for perpetual holding) or presentValue
#' (for exit scenarios) at the terminal year value. The function returns the simulated NPVs
#' and confidence intervals for the results.
#'
#'
#'
#' @param Flow Numeric vector of cash flows for the scenario.
#' @param Occurrence Numeric vector of periods for each cash flow. Must be the same length as Flow.
#' @param NominalRate Numeric value of the nominal discount rate for the scenario.
#' @param TerminalYear Integer representing the terminal year for applying terminal value.
#' @param FutureValue Numeric. The future value of standing timber or property at the terminal year.
#' @param Exit Logical. If TRUE, the user exits the property at the terminal year (no LEV, just present value of the terminal value).
#' @param NumSimulations Integer for the number of simulations to run (default: 1000).
#' @param Seed Integer for setting the seed of random number generation for reproducibility (default: 123).
#' @param SD_Flow Standard deviation percentage for simulating cash flow variations (default: 0.2 or 20\%).
#' @param SD_Discount Standard deviation percentage for simulating discount rate variations (default: 0\%).
#' @param SD_FutureValue Standard deviation percentage for simulating future value variations (default: 0.2 or 20\%).
#'
#' @importFrom stats rnorm quantile
#' @importFrom ggplot2 aes geom_histogram labs theme_minimal
#' @seealso \code{\link{ci_table}} \code{\link{landExpectVal}} \code{\link{simpleNPV}}
#' @return A list containing all simulated NPVs and confidence intervals (68%, 80%, 90%).
#' @examples
#' library(ggplot2)
#'
#' # Exiting the Property (using presentValue)
#' Flow <- c(100, 200, -150, 300)
#' Occurrence <- c(1, 2, 3, 4)
#' NominalRate <- 0.05
#' TerminalYear <- 40
#' FutureValue <- 1960
#'
#' result_exit <- monteCarloAnalysis(Flow, Occurrence, NominalRate,
#' TerminalYear, FutureValue, Exit = TRUE)
#'
#' # View results
#' # result_exit$NPVs           # All simulated NPVs (list length NumSimulations)
#' result_exit$CI_68          # 68% confidence interval
#' result_exit$CI_80          # 80% confidence interval
#' result_exit$CI_90          # 90% confidence interval
#'
#' # Plot the NPV distribution for exiting the property
#' ggplot(data.frame(NPV = result_exit$NPVs), aes(x = NPV)) +
#'   geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
#'   labs(title = "NPV Distribution - Exiting the Property", x = "NPV", y = "Frequency") +
#'   theme_minimal()
#'
#' # Holding the Property (using landExpectVal)
#' result_hold <- monteCarloAnalysis(Flow, Occurrence, NominalRate,
#' TerminalYear, FutureValue, Exit = FALSE)
#'
#' # View results
#' # result_exit$NPVs           # All simulated NPVs (list length NumSimulations)
#' result_hold$CI_68          # 68% confidence interval
#' result_hold$CI_80          # 80% confidence interval
#' result_hold$CI_90          # 90% confidence interval
#'
#' # Plot the NPV distribution for holding the property
#' ggplot(data.frame(NPV = result_hold$NPVs), aes(x = NPV)) +
#'   geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
#'   labs(title = "NPV Distribution - Holding the Property", x = "NPV", y = "Frequency") +
#'   theme_minimal()
#'
#' @export

monteCarloAnalysis <- function(
    Flow, Occurrence, NominalRate, TerminalYear, FutureValue, Exit = FALSE,
    NumSimulations = 5000, Seed = 123, SD_Flow = 0.2, SD_Discount = 0, SD_FutureValue = 0.2
) {
  # Ensure Flow and Occurrence have the same length
  if (length(Flow) != length(Occurrence)) {
    stop("Flow and Occurrence vectors must be of the same length.")
  }

  # Set seed for reproducibility
  set.seed(Seed)

  # Initialize result vector for simulated NPVs
  NPVs <- numeric(NumSimulations)

  # Monte Carlo Simulation
  for (sim in 1:NumSimulations) {
    # Simulate random cash flows (assuming normal distribution)
    SimulatedFlows <- rnorm(length(Flow), mean = Flow, sd = abs(Flow * SD_Flow))

    # Simulate a random discount rate (assuming normal distribution)
    SimulatedRate <- rnorm(1, mean = NominalRate, sd = NominalRate * SD_Discount)

    # Simulate the FutureValue (e.g., value of timber at terminal year)
    SimulatedFutureValue <- rnorm(1, mean = FutureValue, sd = FutureValue * SD_FutureValue)

    # Calculate NPV for regular cash flows using simpleNPV
    npv_cash_flows <- simpleNPV(SimulatedFlows, Occurrence, SimulatedRate)

    # Terminal Value: either exit (use presentValue) or hold (use landExpectVal)
    if (Exit) {
      # Exit at the terminal year: use the present value of the future cash flow
      terminal_value <- presentValue(SimulatedFutureValue, TerminalYear, SimulatedRate)
    } else {
      # Hold the property: use LEV to calculate perpetual value
      terminal_value <- landExpectVal(SimulatedFutureValue, TerminalYear, SimulatedRate)
    }

    # Combine the NPV of cash flows and the terminal value
    NPVs[sim] <- npv_cash_flows + terminal_value
  }

  # Perform Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(NPVs)

  if (shapiro_test$p.value > 0.05) {
    # Data is normally distributed, use parametric confidence intervals

    # Calculate mean and standard deviation
    mean_npv <- mean(NPVs)
    sd_npv <- sd(NPVs)

    # Parametric confidence intervals (based on normal distribution)
    CI_68 <- c(mean_npv - sd_npv, mean_npv + sd_npv)
    CI_80 <- c(mean_npv - 1.28 * sd_npv, mean_npv + 1.28 * sd_npv)  # ~80% CI
    CI_90 <- c(mean_npv - 1.65 * sd_npv, mean_npv + 1.65 * sd_npv)  # ~90% CI
  } else {
    # Data is not normally distributed, use non-parametric quantile-based confidence intervals

    CI_68 <- quantile(NPVs, c(0.16, 0.84))
    CI_80 <- quantile(NPVs, c(0.10, 0.90))
    CI_90 <- quantile(NPVs, c(0.05, 0.95))
  }

  # Return a list containing all NPVs and confidence intervals
  return(list(
    NPVs = NPVs,
    CI_68 = CI_68,
    CI_80 = CI_80,
    CI_90 = CI_90
  ))
}
