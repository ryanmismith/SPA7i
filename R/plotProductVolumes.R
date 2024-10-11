#' Plot Stocking, AAC, and Annual Species Group Standing Inventory
#'
#' @description This function takes the output of the `summarizeProductVolumes` function and creates four plots:
#' 1. A stocking summary showing initial and final standing volumes for each species group.
#' 2. An AAC (Annual Allowable Cut) summary showing initial and final harvest volumes for each species group.
#' 3. A line plot showing the total standing inventory by year for each species group.
#' 4. A line plot showing the AAC by year for each species group.
#'
#' The number of years plotted will be equal to the number of years run in the `run_aac_for_all` function.
#'
#' The species groups include Cedar, Hardwood, Other Softwood, Pine, Poplar, and Spruce-Fir.
#' The function automatically adjusts the y-axis to dynamically scale based on the maximum values in the data.
#'
#' @param summary A list containing annual volumes, summary volumes, and hardwood volumes from the `summarizeProductVolumes` function.
#' @param base_size An integer specifying the base font size for the plot. Defaults to 14.
#'
#' @return A list containing four ggplot objects:
#' 1. `stockingplot`: Stocking summary of initial and final standing volumes for each species group.
#' 2. `aacplot`: AAC summary of initial and final harvest volumes for each species group.
#' 3. `annual_inventory_plot`: Line plot showing annual species group standing inventory.
#' 4. `annual_aac_plot`: Line plot showing annual species group AAC.
#' @family Reporting Functions
#' @seealso \code{\link{summarizeProductVolumes}} for generating the summarized product volumes used as input to this function.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' summary <- summarizeProductVolumes(aac_results, ratios_df)
#' summary_plots <- plotProductVolumes(summary)
#' print(summary_plots$stockingplot)
#' print(summary_plots$aacplot)
#' print(summary_plots$annual_inventory_plot)
#' print(summary_plots$annual_inventory_plot)
#' }
#'
#' @export
plotProductVolumes <- function(summary, base_size = 14) {

  # Automatically extract the final year from the summary for labeling purposes
  final_year_label <- max(summary$summary_volumes$final_year, na.rm = TRUE)

  # Reshape the data to long format for plotting volumes
  volume_long <- summary$summary_volumes %>%
    dplyr::select(species_group, initial_volume, final_volume) %>%
    tidyr::gather(key = "year", value = "volume", -species_group) %>%
    dplyr::mutate(year = ifelse(year == "initial_volume", "Year 1", paste0("Year ", final_year_label)))

  # Reshape the data to long format for plotting harvests
  harvest_long <- summary$summary_volumes %>%
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
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  ## 2. AAC Summary Plot (Harvests)
  aac_plot <- ggplot(harvest_long, aes(x = species_group, y = harvest, fill = year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "white") +
    labs(title = "AAC Summary", x = "Species Group", y = "Total Cords") +
    scale_x_discrete() +
    scale_y_continuous(labels = scales::comma, limits = c(0, y_limit_harvest)) +  # Dynamic Y-axis limit
    scale_fill_manual(values = c("blue", "orange")) +
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
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  # Get the maximum values for dynamic scaling
  max_annual_volume <- max(summary$annual_volumes$total_volume, na.rm = TRUE)
  max_annual_harvest <- max(summary$annual_volumes$total_harvest, na.rm = TRUE)

  # Add buffer to the y-axis limits (e.g., 10% above the maximum value)
  y_limit_annual_volume <- max_annual_volume * 1.1
  y_limit_annual_harvest <- max_annual_harvest * 1.1

  # Create a sequence of years from 1 to the final year
  year_seq <- seq(1, max(summary$annual_volumes$year), 1)

  ## 3. Annual Species Group Standing Inventory Plot (Line Graph)
  annual_inventory_plot <- ggplot(summary$annual_volumes, aes(x = year, y = total_volume, color = species_group)) +
    geom_line(linewidth = 1.1) +
    labs(title = "Annual Species Group Standing Inventory", x = "Year", y = "Total Cords") +
    scale_color_manual(values = c("Cedar" = "green", "Hardwood" = "brown", "Other Softwood" = "purple",
                                  "Pine" = "blue", "Poplar" = "yellow", "Spruce-Fir" = "darkgreen")) +  # Custom colors
    scale_x_continuous(breaks = year_seq) +  # Sequential X-axis years
    scale_y_continuous(labels = scales::comma, limits = c(0, y_limit_annual_volume)) +  # Dynamic Y-axis limit
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "#D3D3D3"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  ## 4. Annual Species Group AAC Inventory Plot (Line Graph)
  annual_aac_plot <- ggplot(summary$annual_volumes, aes(x = year, y = total_harvest, color = species_group)) +
    geom_line(linewidth = 1.1) +
    labs(title = "Annual Species AAC", x = "Year", y = "Total Cords") +
    scale_color_manual(values = c("Cedar" = "green", "Hardwood" = "brown", "Other Softwood" = "purple",
                                  "Pine" = "blue", "Poplar" = "yellow", "Spruce-Fir" = "darkgreen")) +  # Custom colors
    scale_x_continuous(breaks = year_seq) +  # Sequential X-axis years
    scale_y_continuous(labels = scales::comma, limits = c(0, y_limit_annual_harvest)) +  # Dynamic Y-axis limit
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "#D3D3D3"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  # Return all four plots as a list
  return(list(stockingplot = stocking_plot, aacplot = aac_plot,
              annual_inventory_plot = annual_inventory_plot,
              annual_aac_plot = annual_aac_plot))
}

