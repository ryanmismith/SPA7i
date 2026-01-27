#' Monte Carlo NPV Simulation with Time-Varying Volatility (Ornstein-Uhlenbeck)
#'
#' @description
#' Performs Monte Carlo simulations to calculate NPV under uncertainty with
#' volatility that increases over the forecast horizon (Ornstein-Uhlenbeck). Correct approach
#' for modeling long-term uncertainty in timber valuations.
#'
#' @section Time-Varying Volatility:
#' Traditional Monte Carlo simulations assume constant volatility across all years.
#' This is unrealistic for long-term timber investments because forecast uncertainty
#' compounds with time horizon:
#'
#' \itemize{
#'   \item Year 1: Low uncertainty (contracts signed, current market known)
#'   \item Year 10: Moderate uncertainty (market cycles, policy changes)
#'   \item Year 30: High uncertainty (structural shifts, climate impacts)
#' }
#'
#' @section Mean-Reverting Volatility Model (Default):
#' Timber prices revert to long-run biological equilibrium. Uncertainty grows
#' initially but caps at a maximum rather than growing unbounded.
#'
#' The mean-reverting volatility model is:
#' \deqn{\sigma(t) = \sigma_{\infty} \times (1 - e^{-\kappa t})}
#'
#' where:
#' \itemize{
#'   \item \eqn{\sigma_{\infty}} = long-run maximum volatility (e.g., 40%)
#'   \item \eqn{\kappa} = mean-reversion speed (typical: 0.10 to 0.30)
#'   \item \eqn{t} = forecast horizon in years
#' }
#'
#' Example with \eqn{\sigma_{\infty} = 0.40} and \eqn{\kappa = 0.15}:
#' \itemize{
#'   \item Year 1: \eqn{\sigma(1) = 0.40 \times (1 - e^{-0.15}) = 0.056} (5.6%)
#'   \item Year 5: \eqn{\sigma(5) = 0.40 \times (1 - e^{-0.75}) = 0.211} (21.1%)
#'   \item Year 10: \eqn{\sigma(10) = 0.40 \times (1 - e^{-1.5}) = 0.311} (31.1%)
#'   \item Year 20: \eqn{\sigma(20) = 0.40 \times (1 - e^{-3.0}) = 0.380} (38.0%)
#'   \item Year 50: \eqn{\sigma(50) = 0.40 \times (1 - e^{-7.5}) = 0.400} (40.0% - converged)
#' }
#'
#' Why this makes sense for timber:
#' \enumerate{
#'   \item Prices cannot drift infinitely due to biological constraints
#'   \item Supply adjusts to price (high prices lead to more planting)
#'   \item Long-run price tied to replacement cost (land + growth + harvest)
#'   \item Market forces create mean reversion over multi-decade horizons
#' }
#'
#'
#' @section Square Root of Time Scaling (Alternative):
#' Setting \code{use_sqrt_time = TRUE} switches to a simpler model from financial theory:
#' \deqn{\sigma(t) = \sigma_0 \times \sqrt{t}}
#'
#' where \eqn{\sigma_0} is the base annual volatility.
#'
#' Example with \eqn{\sigma_0 = 0.15}:
#' \itemize{
#'   \item Year 1: \eqn{\sigma(1) = 0.15 \times \sqrt{1} = 0.15} (15%)
#'   \item Year 4: \eqn{\sigma(4) = 0.15 \times \sqrt{4} = 0.30} (30%)
#'   \item Year 9: \eqn{\sigma(9) = 0.15 \times \sqrt{9} = 0.45} (45%)
#'   \item Year 25: \eqn{\sigma(25) = 0.15 \times \sqrt{25} = 0.75} (75%)
#'   \item Year 50: \eqn{\sigma(50) = 0.15 \times \sqrt{50} = 1.06} (106% - unrealistic!)
#' }
#'
#' Problem: Volatility grows unbounded. For long-term timber forecasts (20+ years),
#' this produces unrealistically wide confidence intervals.
#'
#' When to use square root scaling:
#' \itemize{
#'   \item Short-to-medium horizons (5-15 years)
#'   \item Academic/teaching purposes (simpler to explain)
#'   \item Comparison/sensitivity testing
#'   \item Financial assets that truly random-walk (NOT timber)
#' }
#'
#' Note: When using \code{use_sqrt_time = TRUE}, set \code{sigma_infinity} to
#' represent base annual volatility (e.g., 0.15) rather than maximum volatility.
#'
#' @section Log-Normal Distributions:
#' This function uses log-normal distributions instead of normal distributions
#' for simulating cash flows. This prevents negative stumpage prices, which can
#' occur with normal distributions in extreme scenarios. Log-normal distributions
#' also better match empirical commodity price distributions (right-skewed,
#' bounded at zero).
#'
#' @param Flow Numeric vector of cash flows
#' @param Occurrence Numeric vector of periods (years) for each cash flow.
#'   Must be same length as Flow.
#' @param NominalRate Numeric discount rate
#' @param TerminalYear Integer terminal year for terminal value
#' @param FutureValue Numeric future value at terminal year
#' @param Exit Logical. If TRUE, exit property at terminal year (present value).
#'   If FALSE, perpetual holding (land expectation value).
#' @param NumSimulations Integer number of Monte Carlo iterations (default: 5000)
#' @param Seed Integer for reproducibility (default: 123)
#' @param sigma_infinity Numeric long-run maximum volatility for mean-reverting
#'   model OR base annual volatility for sqrt model (default: 0.40).
#'   For mean-reversion: typical range 0.35 to 0.50.
#'   For sqrt scaling: typical range 0.10 to 0.20.
#' @param kappa Numeric mean-reversion speed (default: 0.15).
#'   Higher values mean faster approach to maximum. Typical range: 0.10 to 0.30.
#'   Only used when \code{use_sqrt_time = FALSE}.
#' @param use_sqrt_time Logical. If TRUE, use \eqn{\sqrt{t}} volatility scaling
#'   instead of mean-reversion (default: FALSE).
#'   \itemize{
#'     \item FALSE (default): Industry standard mean-reverting model for timber
#'     \item TRUE: Simpler unbounded growth model (use for short horizons or comparison)
#'   }
#' @param SD_Discount Numeric standard deviation for discount rate (default: 0).
#'   Usually kept at 0 (constant discount rate).
#' @param SD_FutureValue Numeric standard deviation for terminal value (default: 0.20)
#'
#' @return A list containing:
#' \describe{
#'   \item{NPVs}{Numeric vector of all simulated NPV values}
#'   \item{CI_68}{68\% confidence interval}
#'   \item{CI_80}{80\% confidence interval}
#'   \item{CI_90}{90\% confidence interval}
#'   \item{volatility_profile}{Data frame with Year and Sigma for each cash flow}
#' }
#'
#' @examples
#' # Example 1: Mean-reverting volatility (INDUSTRY STANDARD)
#' Flow <- c(10000, 12000, 15000, 18000, 20000)
#' Occurrence <- c(1, 5, 10, 20, 30)
#' NominalRate <- 0.05
#' TerminalYear <- 30
#' FutureValue <- 500000
#'
#' result <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = TRUE,
#'   sigma_infinity = 0.40,  # 40% maximum volatility
#'   kappa = 0.15,          # Moderate mean-reversion
#'   NumSimulations = 5000
#' )
#'
#' # View how volatility changes over time
#' print(result$volatility_profile)
#' #   Year Sigma
#' #   1    0.056  (5.6% - low uncertainty)
#' #   5    0.211  (21.1%)
#' #   10   0.311  (31.1%)
#' #   20   0.380  (38.0%)
#' #   30   0.394  (39.4% - approaching max)
#'
#' # View confidence intervals
#' result$CI_68  # 68% confidence interval
#' result$CI_90  # 90% confidence interval
#'
#' \dontrun{
#' # Plot distribution
#' library(ggplot2)
#' ggplot(data.frame(NPV = result$NPVs), aes(x = NPV)) +
#'   geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
#'   labs(title = "NPV Distribution - Mean-Reverting Volatility",
#'        subtitle = paste("sigma_infinity = 0.40, kappa = 0.15")) +
#'   theme_minimal()
#' }
#'
#' # Example 2: Square-root-of-time (for comparison)
#' result_sqrt <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = TRUE,
#'   sigma_infinity = 0.15,   # 15% BASE volatility (much lower!)
#'   use_sqrt_time = TRUE,
#'   NumSimulations = 5000
#' )
#'
#' print(result_sqrt$volatility_profile)
#' #   Year Sigma
#' #   1    0.150  (15%)
#' #   5    0.335  (33.5%)
#' #   10   0.474  (47.4%)
#' #   20   0.671  (67.1% - very high)
#' #   30   0.822  (82.2% - unrealistically high)
#'
#' # Example 3: Sensitivity to kappa (mean-reversion speed)
#' # Fast mean-reversion (kappa = 0.30): Uncertainty peaks quickly
#' result_fast <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = TRUE, sigma_infinity = 0.40, kappa = 0.30
#' )
#'
#' # Slow mean-reversion (kappa = 0.10): Uncertainty grows gradually
#' result_slow <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = TRUE, sigma_infinity = 0.40, kappa = 0.10
#' )
#'
#' # Compare year 10 volatility:
#' result_fast$volatility_profile[3, ]  # kappa=0.30: sigma(10) ~ 38%
#' result_slow$volatility_profile[3, ]  # kappa=0.10: sigma(10) ~ 25%
#'
#' # Example 4: Exit vs. Hold scenarios
#' # Exit: Sell property at terminal year
#' result_exit <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = TRUE  # Liquidate at year 30
#' )
#'
#' # Hold: Continue operations indefinitely (LEV perpetuity)
#' result_hold <- monteCarloAnalysis(
#'   Flow, Occurrence, NominalRate, TerminalYear, FutureValue,
#'   Exit = FALSE  # Perpetual holding
#' )
#'
#' # Compare NPV distributions
#' mean(result_exit$NPVs)  # Exit NPV
#' mean(result_hold$NPVs)  # Hold NPV (typically higher due to perpetuity)
#'
#' @export

monteCarloAnalysis_OU <- function(
    Flow, Occurrence, NominalRate, TerminalYear, FutureValue, Exit = FALSE,
    NumSimulations = 2000, Seed = 123,
    sigma_infinity = 0.40,    # 40% long-run volatility (industry standard)
    kappa = 0.15,            # Mean reversion speed (industry standard)
    use_sqrt_time = FALSE,
    SD_Discount = 0,
    SD_FutureValue = 0.20
) {
  # Validate inputs
  if (length(Flow) != length(Occurrence)) {
    stop("Flow and Occurrence vectors must be of the same length.")
  }

  # Set seed
  set.seed(Seed)

  # Initialize results
  NPVs <- numeric(NumSimulations)

  # Pre-calculate time-varying volatilities for each cash flow
  # This is the KEY improvement over the original code
  sigma_t <- numeric(length(Flow))

  for (i in 1:length(Flow)) {
    t <- Occurrence[i]  # Years from present

    if (use_sqrt_time) {
      # Simple square-root-of-time scaling (Brownian motion)
      # σ(t) = σ₀ × √t
      # We use sigma_infinity as base volatility here
      sigma_t[i] <- sigma_infinity * sqrt(t)

    } else {
      # Mean-reverting (Ornstein-Uhlenbeck) - INDUSTRY STANDARD
      # σ(t) = σ_∞ × (1 - e^(-λt))
      sigma_t[i] <- sigma_infinity * (1 - exp(-kappa * t))
    }

    # Cap volatility at 100% as safety (only relevant for sqrt_time with long horizons)
    sigma_t[i] <- min(sigma_t[i], 1.0)
  }

  # Monte Carlo simulation loop
  for (sim in 1:NumSimulations) {

    # Simulate cash flows with TIME-VARYING volatility
    SimulatedFlows <- numeric(length(Flow))

    for (i in 1:length(Flow)) {
      # Use LOG-NORMAL distribution (prevents negative prices)
      # For log-normal: if X ~ lognormal(μ, σ), then:
      #   E[X] = exp(μ + σ²/2)
      #   We want E[X] = Flow[i], so:
      #   μ = log(Flow[i]) - σ²/2

      mu_log <- log(abs(Flow[i])) - 0.5 * sigma_t[i]^2

      # Simulate from log-normal
      simulated_value <- rlnorm(1, meanlog = mu_log, sdlog = sigma_t[i])

      # Preserve sign (revenue vs cost)
      SimulatedFlows[i] <- sign(Flow[i]) * simulated_value
    }

    # Simulate discount rate (usually kept constant)
    SimulatedRate <- rnorm(1, mean = NominalRate, sd = NominalRate * SD_Discount)

    # Simulate terminal value
    mu_fv <- log(FutureValue) - 0.5 * SD_FutureValue^2
    SimulatedFutureValue <- rlnorm(1, meanlog = mu_fv, sdlog = SD_FutureValue)

    # Calculate NPV of cash flows
    npv_cash_flows <- simpleNPV(SimulatedFlows, Occurrence, SimulatedRate)

    # Terminal value (exit vs hold)
    if (Exit) {
      terminal_value <- presentValue(SimulatedFutureValue, TerminalYear, SimulatedRate)
    } else {
      terminal_value <- landExpectVal(SimulatedFutureValue, TerminalYear, SimulatedRate)
    }

    # Total NPV
    NPVs[sim] <- npv_cash_flows + terminal_value
  }

  # Test for normality
  shapiro_test <- shapiro.test(NPVs)

  if (shapiro_test$p.value > 0.05) {
    # Parametric confidence intervals
    mean_npv <- mean(NPVs)
    sd_npv <- sd(NPVs)

    CI_68 <- c(mean_npv - sd_npv, mean_npv + sd_npv)
    CI_80 <- c(mean_npv - 1.28 * sd_npv, mean_npv + 1.28 * sd_npv)
    CI_90 <- c(mean_npv - 1.65 * sd_npv, mean_npv + 1.65 * sd_npv)

  } else {
    # Non-parametric quantile-based intervals
    CI_68 <- quantile(NPVs, c(0.16, 0.84))
    CI_80 <- quantile(NPVs, c(0.10, 0.90))
    CI_90 <- quantile(NPVs, c(0.05, 0.95))
  }

  # Return results with volatility profile for transparency
  return(list(
    NPVs = NPVs,
    CI_68 = CI_68,
    CI_80 = CI_80,
    CI_90 = CI_90,
    volatility_profile = data.frame(
      Year = Occurrence,
      Sigma = round(sigma_t, 3)
    )
  ))
}
