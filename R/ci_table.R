#' Table of Confidence Intervals
#'
#' This function generates a table of confidence intervals for a given vector of potential values.
#' The function calculates confidence intervals at the 68, 80, and 90 percent levels. The input can either
#' be a list generated by the `monteCarloAnalysis` function within the same package or a numeric vector
#' of potential values from an independent simulation.
#'
#' If a list is passed, the function extracts the `NPVs` element from the list. If a numeric vector
#' is provided, it uses the values directly. The function first checks if the data is normally distributed
#' using the Shapiro-Wilk test. If the data is normally distributed (p-value > 0.05),
#' parametric confidence intervals are calculated based on the mean and standard deviation, using:
#'
#' - 1 standard deviation for the 68 percent confidence interval
#' - 1.28 standard deviations for the 80 percent confidence interval
#' - 1.65 standard deviations for the 90 percent confidence interval
#'
#' If the data is not normally distributed (p-value <= 0.05), non-parametric confidence intervals are calculated using quantiles:
#'
#' - 68 percent confidence interval using the 16th and 84th percentiles
#' - 80 percent confidence interval using the 10th and 90th percentiles
#' - 90 percent confidence interval using the 5th and 95th percentiles
#'
#' @param npv A list containing the element `NPVs` from the `monteCarloAnalysis` function, or a numeric vector of potential NPV values.
#'
#' @return A data frame containing the confidence intervals (68, 80, 90 percent) and their respective lower and upper bounds.
#'
#' @examples
#' # Example with a numeric vector from a Monte Carlo simulation (Normal Distribution)
#' set.seed(123)
#' npv_values <- rnorm(1000, mean = 200000, sd = 25000)
#' ci_table(npv_values)
#'
#' # Example with a list from monteCarloAnalysis (Normal Distribution)
#' result <- list(NPVs = rnorm(1000, mean = 200000, sd = 25000),
#'   element1 = c(23, 23),
#'   element2 = c(32, 32)
#'   )
#' ci_table(result)
#'
#' # Example with a numeric vector from an Exponential distribution (Not Normally Distributed)
#' set.seed(123)
#' npv_values_skewed <- rexp(1000, rate = 1/200000)
#' ci_table(npv_values_skewed)
#'
#' @family Summary Functions
#'
#' @seealso \code{\link{monteCarloAnalysis}}
#'
#' @references Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). Biometrika, 52(3/4), 591-611.
#'
#' @export

ci_table <- function(npv) {
  if (is.list(npv)) {
    npv_values <- npv$NPVs
  } else {
    npv_values <- npv
  }

  # Perform Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(npv_values)

  if (shapiro_test$p.value > 0.05) {
    # Data is normally distributed, use parametric confidence intervals
    message("Data is normally distributed. Using parametric confidence intervals.")

    # Calculate mean and standard deviation
    mean_npv <- mean(npv_values)
    sd_npv <- sd(npv_values)

    # Parametric confidence intervals (based on normal distribution)
    CI_68 <- c(mean_npv - sd_npv, mean_npv + sd_npv)
    CI_80 <- c(mean_npv - 1.28 * sd_npv, mean_npv + 1.28 * sd_npv)  # ~80% CI
    CI_90 <- c(mean_npv - 1.65 * sd_npv, mean_npv + 1.65 * sd_npv)  # ~90% CI
  } else {
    # Data is not normally distributed, use non-parametric quantile-based confidence intervals
    message("Data is not normally distributed. Using non-parametric confidence intervals.")

    CI_68 <- quantile(npv_values, c(0.16, 0.84))
    CI_80 <- quantile(npv_values, c(0.10, 0.90))
    CI_90 <- quantile(npv_values, c(0.05, 0.95))
  }

  # Return a data frame containing the confidence intervals
  ci_table <- data.frame(
    CI = c("68%", "80%", "90%"),
    Lower_Bound = round(c(CI_68[1], CI_80[1], CI_90[1]), 0),
    Upper_Bound = round(c(CI_68[2], CI_80[2], CI_90[2]), 0),
    row.names = NULL
  )

  return(ci_table)
}
