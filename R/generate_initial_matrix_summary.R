#' Generate Initial Matrix Standing Volume Summary Tables
#'
#' @description This function generates summary tables for the initial standing volume for each township-matrix combination at the beginning of the simulation period.
#'
#' @param arcOutput The township and matrix product volumes dataframe exported from ArcGIS.
#' @param aac_results A dataframe containing AAC simulation results (output from `run_aac_for_all` function).
#'
#' @return A dataframe summarizing final standing volumes for each township-matrix combination.
#' @family Summary Functions
#' @examples
#' # Create a sample dataframe for aac_results
#' aac_results <- data.frame(
#'   Year = rep(2023:2025, each = 6),
#'   township = rep(c("Township1", "Township2"), each = 3, times = 3),
#'   matrix = rep(c("MatrixA", "MatrixB", "MatrixC"), times = 6),
#'   HW_Volume = c(100, 150, 200, 120, 180, 250, 140, 190, 220, 160, 210, 230,
#'                 110, 160, 190, 130, 175, 245),
#'   SW_Volume = c(300, 350, 400, 320, 370, 420, 340, 390, 440, 360, 410, 460,
#'                 310, 360, 410, 330, 380, 430),
#'   Total_Volume = c(400, 500, 600, 440, 550, 670, 480, 580, 660, 520, 620, 690,
#'                    420, 520, 600, 460, 555, 675)
#' )
#'
#' # Print the dataframe
#' print(aac_results)
#' \dontrun{
#' # Assuming aac_results is obtained from run_aac_for_all function
#' initial_standing_summary <- generate_initial_matrix_summary(arcOutput, aac_results)
#' print(initial_standing_summary)
#' }
#' @import dplyr
#' @export

generate_initial_matrix_summary <- function(arcOutput, aac_results) {
  first_year <- min(aac_results$Year)

  # Compute initial standing volumes
  initial_standing <- aac_results %>%
    filter(Year == first_year) %>%
    group_by(township, matrix) %>%
    summarise(
      initial_HW_Volume = mean(HW_Volume, na.rm = TRUE),
      initial_SW_Volume = mean(SW_Volume, na.rm = TRUE),
      initial_Total_Volume = mean(Total_Volume, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute annual growth
  growth <- aac_results %>%
    filter(Year == first_year + 1) %>%
    group_by(township, matrix) %>%
    summarise(
      total_annual_growth = mean(total_growth, na.rm = TRUE),
      .groups = "drop"
    )

  # Combine standing and growth data
  stocking <- left_join(initial_standing, growth, by = c("township", "matrix"))

  # Get acres by Township/Matrix
  acres <- arcOutput %>%
    group_by(township, matrix) %>%
    summarise(acres = mean(acres, na.rm = TRUE), .groups = "drop")

  # Combine with acreage data
  data <- left_join(stocking, acres, by = c("township", "matrix"))

  # Compute total volumes and growth, and select required columns
  data <- data %>%
    mutate(
      total_hw_vol = initial_HW_Volume * acres,
      total_sw_vol = initial_SW_Volume * acres,
      total_growth = total_annual_growth * acres,
      total_vol = total_hw_vol + total_sw_vol
    ) %>%
    select(township, matrix, total_hw_vol, total_sw_vol, total_vol, total_growth, acres)

  return(data)
}

