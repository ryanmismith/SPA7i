#' Unified Monte Carlo NPV Simulation with Time-Dependent Stochastic Processes
#'
#' This function performs a Monte Carlo simulation to estimate the Net Present Value (NPV)
#' of a cash flow stream. It supports three stochastic processes, two of which (`OU` and `walk`)
#' are \strong{time-dependent}, meaning they simulate a single market path that evolves over time.
#'
#' @section Critical Warning - Correlated Flows:
#' For `OU` and `walk` processes, this function simulates \strong{one single market timeline} per simulation.
#' \itemize{
#'   \item \strong{Shared Deviations:} All cash flows occurring in the same year (e.g., Year 5 Timber Revenue AND Year 5 Hunting Lease) will receive the \strong{exact same percentage deviation}.
#'   \item \strong{Implication:} The function assumes all flows in the `Flow` vector are perfectly correlated (e.g., they are all driven by the same "Timber Price" index).
#'   \item \strong{How to Model Uncorrelated Assets:} If you have two uncorrelated revenue streams (e.g., Timber vs. Carbon Credits), you must run this function twice (once for each stream, using different `Seed` values) and sum the resulting NPV vectors.
#' }
#'
#' @section Process Types:
#' The \code{ProcessType} argument controls the simulation logic:
#'
#' \strong{1. "normal" (Independent Log-Normal)}
#' \itemize{
#'   \item \strong{Logic:} Index-based. Every single entry in the `Flow` vector gets a unique, independent random shock, even if they happen in the same year.
#'   \item \strong{Use Case:} Idiosyncratic risks (e.g., machine breakdowns, one-off costs) where Year 5's expense has no relationship to Year 5's revenue.
#' }
#'
#' \strong{2. "OU" (Ornstein-Uhlenbeck / Mean Reversion)}
#' \itemize{
#'   \item \strong{Logic:} Time-based. The process evolves annually. If multiple flows occur in Year 5, the model advances the stochastic process to Year 5 \emph{once}, generates the market price deviation, and applies that \emph{same deviation} to all Year 5 flows.
#'   \item \strong{Use Case:} Commodity price modeling (Timber). High prices in Year 4 lead to mean-reverting pressure in Year 5, affecting all harvest activities in that year equally.
#' }
#'
#' \strong{3. "walk" (Geometric Random Walk)}
#' \itemize{
#'   \item \strong{Logic:} Time-based. Similar to OU, the random walk evolves by year. Cumulative volatility grows with time. Includes a Jensen's Inequality correction to prevent artificial upward value drift.
#'   \item \strong{Use Case:} Inflationary costs or speculative asset prices where today's value is the baseline for tomorrow, and all cash flows in a given year face the same market conditions.
#' }
#'
#' @param Flow Numeric vector of projected cash flows.
#' @param Occurrence Numeric vector of years for each flow.
#'   \strong{Crucial for OU/Walk:} The vector must be sorted or grouped by year for the time-step logic to work most efficiently, though the code handles unsorted inputs by tracking `current_year`.
#'   If \code{NULL}, defaults to Year 1 for all flows.
#' @param NominalRate Numeric. The nominal discount rate (e.g., 0.05).
#' @param ProcessType Character. \code{"normal"}, \code{"OU"}, or \code{"walk"}.
#' @param NumSimulations Integer. Number of iterations (default 2000).
#' @param Seed Integer. Sets the random seed. Change this when summing results from different asset classes to ensure independence.
#' @param ApplyDiscounting Logical. If \code{FALSE}, sets `NominalRate` to 0.
#' @param SD_Flow Numeric. Volatility parameter (Annual Standard Deviation).
#' @param SD_Discount Numeric. Standard deviation for the discount rate.
#' @param sigma_infinity Numeric. (OU Only) Long-run volatility cap.
#' @param lambda Numeric. (OU Only) Mean reversion speed.
#'
#' @return A list containing:
#' \describe{
#'   \item{NPVs}{Vector of simulated NPVs.}
#'   \item{CI_68/80/90}{Confidence intervals.}
#'   \item{variance_profile}{Data frame showing the theoretical risk evolution over time.}
#' }
#'
#' @examples
#' # --- Example: Timber (OU) + Independent Costs (Normal) ---
#'
#' # 1. Define Timber Revenue (Correlated market risk)
#' timber_flows <- c(5000, 15000)
#' timber_years <- c(10, 20)
#'
#' sim_timber <- monteCarloAnalysis(
#'   Flow = timber_flows, Occurrence = timber_years, NominalRate = 0.05,
#'   ProcessType = "OU", Seed = 123
#' )
#'
#' # 2. Define Road Costs (Independent operational risk)
#' cost_flows <- rep(-500, 20)
#' cost_years <- 1:20
#'
#' sim_costs <- monteCarloAnalysis(
#'   Flow = cost_flows, Occurrence = cost_years, NominalRate = 0.05,
#'   ProcessType = "normal", Seed = 456 # Different seed!
#' )
#'
#' # 3. Combine for Total Project NPV
#' total_npv <- sim_timber$NPVs + sim_costs$NPVs
#' quantile(total_npv, c(0.1, 0.9))
#'
#' @export

monteCarloAnalysis <- function(
    Flow,
    Occurrence = NULL,
    NominalRate,
    ProcessType = "normal",
    NumSimulations = 2000,
    Seed = 123,
    ApplyDiscounting = TRUE,
    SD_Flow = 0.20,
    SD_Discount = 0,
    sigma_infinity = 0.40,
    lambda = 0.15
) {

  # --- 1. HANDLE OCCURRENCE DEFAULTS ---
  if (is.null(Occurrence)) {
    Occurrence <- rep(1, length(Flow))
    message("Note: No Occurrence vector provided. Assuming Year 1 for all flows.")
  } else if (length(Occurrence) < length(Flow)) {
    len_diff <- length(Flow) - length(Occurrence)
    Occurrence <- c(Occurrence, rep(1, len_diff))
    message(sprintf("Note: Occurrence vector shorter than Flow. Appended %d entries as Year 1.", len_diff))
  } else if (length(Occurrence) > length(Flow)) {
    stop("Occurrence vector is longer than Flow vector.")
  }

  # --- 2. HANDLE DISCOUNTING ---
  original_rate <- NominalRate
  if (!ApplyDiscounting) {
    NominalRate <- 0
  }

  # --- 3. PROCESS VALIDATION ---
  if (!ProcessType %in% c("normal", "OU", "walk")) {
    stop("ProcessType must be 'normal', 'OU', or 'walk'")
  }

  # --- 4. SETUP ---
  set.seed(Seed)
  NPVs <- numeric(NumSimulations)
  debug_flows <- NULL

  # Pre-calculate constants for OU
  reversion_factor <- exp(-lambda)
  ou_innovation <- sigma_infinity * sqrt(1 - exp(-2 * lambda))

  # --- 5. SIMULATION LOOP ---
  for (sim in 1:NumSimulations) {

    SimulatedFlows <- numeric(length(Flow))

    if (ProcessType == "normal") {
      # --- NORMAL (Independent) - INDEX-BASED ---
      # Each flow is independent, no temporal correlation
      SimulatedFlows <- rnorm(length(Flow), mean = Flow, sd = abs(Flow * SD_Flow))

    } else if (ProcessType == "OU") {
      # --- OU (Mean Reverting) - TIME-BASED ---
      current_log_deviation <- 0
      current_year <- 0

      for (i in 1:length(Flow)) {
        # Advance the OU process if we've moved to a new year
        if (Occurrence[i] > current_year) {
          years_elapsed <- Occurrence[i] - current_year

          # Step the OU process forward for each year that passed
          for (yr in 1:years_elapsed) {
            drifted_deviation <- current_log_deviation * reversion_factor
            shock <- rnorm(1, mean = 0, sd = ou_innovation)
            current_log_deviation <- drifted_deviation + shock
          }

          current_year <- Occurrence[i]
        }

        # All flows in the same year use the SAME deviation
        SimulatedFlows[i] <- Flow[i] * exp(current_log_deviation - 0.5 * sigma_infinity^2)
      }

    } else if (ProcessType == "walk") {
      # --- RANDOM WALK (Cumulative) - TIME-BASED ---
      cumulative_log_deviation <- 0
      current_year <- 0
      current_step <- 0  # Track how many steps we've taken

      for (i in 1:length(Flow)) {
        # Advance the random walk if we've moved to a new year
        if (Occurrence[i] > current_year) {
          years_elapsed <- Occurrence[i] - current_year

          # Step the random walk forward for each year that passed
          for (yr in 1:years_elapsed) {
            shock <- rnorm(1, mean = 0, sd = SD_Flow)
            cumulative_log_deviation <- cumulative_log_deviation + shock
            current_step <- current_step + 1
          }

          current_year <- Occurrence[i]
        }

        # Calculate the correction based on how many steps we've taken
        cumulative_variance <- current_step * SD_Flow^2

        # All flows in the same year use the SAME deviation
        SimulatedFlows[i] <- Flow[i] * exp(cumulative_log_deviation - 0.5 * cumulative_variance)
      }
    }

    # Capture first iteration for debugging
    if (sim == 1) debug_flows <- SimulatedFlows

    # --- SIMULATE DISCOUNT RATE ---
    SimulatedRate <- rnorm(1, mean = NominalRate, sd = NominalRate * SD_Discount)

    # --- CALCULATE NPV ---
    NPVs[sim] <- simpleNPV(SimulatedFlows, Occurrence, SimulatedRate)
  }

  # --- 6. STATS & OUTPUT ---
  # Only run statistics if we have enough data (>= 3 samples)
  if (length(NPVs) >= 3) {
  shapiro_test <- if(length(NPVs) > 5000) shapiro.test(sample(NPVs, 5000)) else shapiro.test(NPVs)

  if (shapiro_test$p.value > 0.05) {
    mean_val <- mean(NPVs)
    sd_val <- sd(NPVs)
    CI_68 <- c(mean_val - sd_val, mean_val + sd_val)
    CI_80 <- c(mean_val - 1.28 * sd_val, mean_val + 1.28 * sd_val)
    CI_90 <- c(mean_val - 1.65 * sd_val, mean_val + 1.65 * sd_val)
    method_used <- "Parametric (Normal)"
  } else {
    CI_68 <- quantile(NPVs, c(0.16, 0.84))
    CI_80 <- quantile(NPVs, c(0.10, 0.90))
    CI_90 <- quantile(NPVs, c(0.05, 0.95))
    method_used <- "Non-Parametric (Quantile)"
   }
  } else {
  # Fallback for 1 or 2 simulations (Validation Mode)
  # Just return the values themselves as the "CI"
  CI_68 <- range(NPVs)
  CI_80 <- range(NPVs)
  CI_90 <- range(NPVs)
  method_used <- "Exact Value (Validation Mode)"
}

  cat("\n=== Monte Carlo Simulation Debug ===\n")
  cat(sprintf("Process: %s | Seed: %d | Discounting: %s\n",
              ProcessType, Seed, ifelse(ApplyDiscounting, "ON", "OFF")))
  cat("First Iteration Check:\n")
  print(head(data.frame(Year=Occurrence, Projected=round(Flow,0),
                        Simulated=round(debug_flows,0),
                        Diff_Pct=round((debug_flows-Flow)/Flow*100,1)), 10))
  cat("====================================\n")

  # --- 7. VARIANCE PROFILES ---
  variance_profile <- NULL

  if (ProcessType == "OU") {
    # OU: Variance approaches sigma_infinity^2 asymptotically
    # Use unique years for the profile
    unique_years <- unique(Occurrence)
    variance_profile <- data.frame(
      Year = unique_years,
      Sigma = round(sigma_infinity * sqrt(1 - exp(-2 * lambda * unique_years)), 4),
      Correction = round(0.5 * sigma_infinity^2, 4)  # Constant for OU
    )
  } else if (ProcessType == "walk") {
    # Random Walk: Variance grows linearly with time steps
    # Use unique years for the profile
    unique_years <- unique(Occurrence)
    # For random walk, each unique year represents one step
    step_number <- seq_along(unique_years)
    cumulative_variance <- step_number * SD_Flow^2
    variance_profile <- data.frame(
      Year = unique_years,
      Step = step_number,
      Sigma = round(sqrt(cumulative_variance), 4),
      Variance = round(cumulative_variance, 4),
      Correction = round(0.5 * cumulative_variance, 4)
    )
  }

  return(list(
    NPVs = NPVs,
    CI_68 = CI_68,
    CI_80 = CI_80,
    CI_90 = CI_90,
    process_type = ProcessType,
    variance_profile = variance_profile
  ))
}
