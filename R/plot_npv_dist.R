#' Plot NPV Distribution with Professional Styling
#'
#' @description This function takes the output of the `monteCarloAnalysis` function and creates a professional-quality plot of the NPV distribution,
#' suitable for publication. The plot includes the mean NPV, optional 80 percent confidence intervals, and a customizable title.
#' The plot also allows an optional caption to display the Mean NPV and 80 percent Confidence Interval (CI) lower and upper bound values.
#' There is also an optional legend.
#'
#' @param npv_results A dataframe containing NPV simulation results, which must include a column `NPV` for the net present value and a `CI_80` column for the 80 percent confidence intervals.
#' @param plot_title A string specifying the title of the plot. Defaults to NULL.
#' @param caption A logical value indicating whether to include the Mean NPV and 80 percent CIs in the caption. Defaults to TRUE.
#' @param legend A logical value indicating whether to include a legend for the lines representing the mean NPV and 80 percent CIs. Defaults to FALSE.
#' @param binwidth You may manually adjust the histogram's binwidth, but it defaults a function that is the product of the max NPV value. Defaults to NULL.
#' @param base_size You may change the base_size of the figure axis text if formatting appears irregular. Defaults to 14.
#'
#' @return A ggplot object representing the NPV distribution plot.
#' @family Reporting Functions
#' @seealso \code{\link{monteCarloAnalysis}}
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs geom_vline scale_x_continuous scale_color_manual theme theme_minimal element_text element_line element_blank element_rect
#' @importFrom scales dollar_format comma
#'
#' @examples
#' # Mock sample list for npv_results
#' # Generate NPV data
#'  npv_values <- rnorm(500, mean = 110440, sd = 22300)  # Random NPV values with normal distribution
#'
#'  # Create the list with NPV and confidence intervals
#'  npv_results <- list(
#'  NPV = npv_values,  # Random NPV values
#'  CI_80 = quantile(npv_values, c(0.10, 0.90))  # 80% confidence intervals at 10th and 90th percentiles
#' )
#'
#' # Generate the NPV plot using the plotNPVDistribution function (no title)
#' npv_plot <- plotNPVDistribution(npv_results, caption = TRUE, legend = TRUE)
#'
#' # Print the generated plot
#' print(npv_plot)
#'
#' # Example 2: Generate the NPV plot using the plotNPVDistribution function (no caption or legend)
#' npv_plot2 <- plotNPVDistribution(npv_results, "Simulated Parcel Value", caption = FALSE)
#' print(npv_plot2)
#'
#' @export

plotNPVDistribution <- function(npv_results, plot_title = NULL, caption = TRUE, legend = FALSE,
                                binwidth = NULL, base_size = 14) {
  # Ensure that npv_results has a numeric NPV column
  if (!is.numeric(npv_results$NPV)) {
    stop("The NPV column in npv_results must be numeric.")
  }



  # Extract NPV data
  npv_values <- npv_results$NPV
  mean_NPV <- mean(npv_values, na.rm = TRUE)

  # Extract confidence interval data
  ci_lower <- npv_results$CI_80[1]
  ci_upper <- npv_results$CI_80[2]

  # Create a data frame for plotting
  plot_data <- data.frame(NPV = npv_values)

  # Calculate binwidth

  if (!is.null(binwidth)) {
    binwidth <- binwidth
  } else if (max(npv_values) > 250000) {
    binwidth <- round(max(npv_values)/50, -4)
  } else {
    binwidth <- round(max(npv_values)/50, -3)
  }


  # Generate the text for caption
  mean_npv_text <- paste0("Mean NPV: $", scales::comma(round(mean_NPV)))
  ci_text <- paste0("80% Confidence Intervals - Lower: $", scales::comma(round(ci_lower)),
                    ", Upper: $", scales::comma(round(ci_upper)))

  # Calculate the necessary padding to align the two strings
  max_length <- max(nchar(mean_npv_text), nchar(ci_text))
  mean_npv_text <- sprintf("%-*s", max_length, mean_npv_text)
  ci_text <- sprintf("%-*s", max_length, ci_text)

  # Initialize the base plot
  npv_plot <- ggplot2::ggplot(plot_data, aes(x = NPV)) +
    # Histogram
    ggplot2::geom_histogram(binwidth = binwidth, fill = "#0072B2", alpha = 0.8, color = "white") +

    # X-axis formatting
    ggplot2::scale_x_continuous(labels = scales::dollar_format()) +

    # Plot labels and title
    ggplot2::labs(title = plot_title, x = "Net Present Value (NPV)", y = "Frequency") +

    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "#D3D3D3"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white")
    )

  # Conditionally add the Mean NPV and 80% CI Lines
  if (legend) {
    # Use color aesthetic for legend
    npv_plot <- npv_plot +
      ggplot2::geom_vline(aes(xintercept = mean_NPV, color = "Mean NPV"), linetype = "dashed", linewidth = 1.5) +
      ggplot2::geom_vline(aes(xintercept = ci_lower, color = "80% CIs"), linetype = "solid", linewidth = 1) +
      ggplot2::geom_vline(aes(xintercept = ci_upper, color = "80% CIs"), linetype = "solid", linewidth = 1)
  } else {
    # No color aesthetic, direct color mapping
    npv_plot <- npv_plot +
      ggplot2::geom_vline(aes(xintercept = mean_NPV), color = "darkorange", linetype = "dashed", linewidth = 1.5) +  # Mean NPV line (dark teal)
      ggplot2::geom_vline(aes(xintercept = ci_lower), color = "#7570b3", linetype = "solid", linewidth = 1) +  # 80% CI lower bound (dark purple)
      ggplot2::geom_vline(aes(xintercept = ci_upper), color = "#7570b3", linetype = "solid", linewidth = 1)    # 80% CI upper bound (dark purple)
  }

  # Add caption if caption = TRUE
  if (caption) {
    npv_plot <- npv_plot +
      ggplot2::labs(
        caption = paste0(mean_npv_text, "\n", ci_text)
      ) +
      ggplot2::theme(
        plot.caption = element_text(size = 10, hjust = 1, face = "italic") # Align caption to the right
      )
  }

  # Add legend if legend = TRUE
  if (legend) {
    npv_plot <- npv_plot +
      ggplot2::scale_color_manual(
        name = "Lines",  # Legend title
        values = c("Mean NPV" = "darkorange", "80% CIs" = "#7570b3"),  # Custom colors for legend
        labels = c("80% CIs", "Mean NPV")  # Custom labels
      ) +
      ggplot2::theme(
        legend.position = "right",  # Place the legend on the right side
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8)
      )
  }

  return(npv_plot)
}
