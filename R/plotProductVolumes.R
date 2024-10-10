#' Plot Stocking and AAC Volumes
#'
#' @description This function takes the output of the `summarizeProductVolumes` function and creates two plots
#' to visualize the initial and final stocking volumes and AAC harvests for different species groups. The plots are suitable for publication
#' and display the data side-by-side for easy comparison.
#'
#' The species groups are Cedar, Hardwood, Other Softwood, Pine, Poplar, and Spruce-Fir.
#'
#' The function automatically adjusts the y-axis to dynamically scale based on the maximum values in the data.
#'
#' @param summary_volumes A dataframe containing summarized product volumes from the `summarizeProductVolumes` function.
#' @param base_size An integer specifying the base font size for the plot. Defaults to 14.
#'
#' @return A list containing two ggplot objects: one for the stocking summary (`stockingplot`) and one for the AAC summary (`aacplot`).
#' @family Reporting Functions
#' @seealso \code{\link{summarizeProductVolumes}} for generating the summarized product volumes used as input to this function.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assume `aac_results` and `ratios_df` contain the appropriate data.
#' summary_volumes <- summarizeProductVolumes(aac_results, ratios_df)
#'
#' # Generate the plots
#' summary_plots <- plotProductVolumes(summary_volumes)
#'
#' # Print the generated plots
#' print(summary_plots$stockingplot)
#' print(summary_plots$aacplot)
#' }
#' @export
#'
plotProductVolumes <- function(summary_volumes, base_size = 14) {

  # Automatically extract the final year from the summary for labeling purposes
  final_year_label <- max(summary_volumes$final_year, na.rm = TRUE)

  # Reshape the data to long format for plotting volumes
  volume_long <- summary_volumes %>%
    dplyr::select(species_group, initial_volume, final_volume) %>%
    tidyr::gather(key = "year", value = "volume", -species_group) %>%
    dplyr::mutate(year = ifelse(year == "initial_volume", "Year 1", paste0("Year ", final_year_label)))

  # Reshape the data to long format for plotting harvests
  harvest_long <- summary_volumes %>%
    dplyr::select(species_group, initial_harvest, final_harvest) %>%
    tidyr::gather(key = "year", value = "harvest", -species_group) %>%
    dplyr::mutate(year = ifelse(year == "initial_harvest", "AAC Year 1", paste0("AAC Year ", final_year_label)))

  # Get the maximum values for dynamic scaling
  max_volume <- max(volume_long$volume, na.rm = TRUE)
  max_harvest <- max(harvest_long$harvest, na.rm = TRUE)

  # Add buffer to the y-axis limits (e.g., 10% above the maximum value)
  y_limit_volume <- max_volume * 1.1
  y_limit_harvest <- max_harvest * 1.1

  ## 1. Stocking Summary Plot (Volumes)
  stocking_plot <- ggplot(volume_long, aes(x = species_group, y = volume, fill = year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "white") +
    labs(title = "Stocking Summary", x = "Species Group", y = "Total Cords") +
    scale_x_discrete() +
    scale_y_continuous(labels = scales::comma, limits = c(0, y_limit_volume)) +  # Dynamic Y-axis limit
    scale_fill_manual(values = c("blue", "orange")) +  # Custom colors
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Adjusted text angle to 30 degrees
      panel.grid.major = element_line(color = "#D3D3D3"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 10)
    )

  ## 2. AAC Summary Plot (Harvests)
  aac_plot <- ggplot(harvest_long, aes(x = species_group, y = harvest, fill = year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "white") +
    labs(title = "AAC Summary", x = "Species Group", y = "Total Cords") +
    scale_x_discrete() +
    scale_y_continuous(labels = scales::comma, limits = c(0, y_limit_harvest)) +  # Dynamic Y-axis limit
    scale_fill_manual(values = c("blue", "orange")) +  # Custom colors
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Adjusted text angle to 30 degrees
      panel.grid.major = element_line(color = "#D3D3D3"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 10)
    )

  # Return the two plots as a list
  return(list(stockingplot = stocking_plot, aacplot = aac_plot))
}

