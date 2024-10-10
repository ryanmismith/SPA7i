#' Generate Final Standing Volume Summary Tables
#'
#' @description This function generates summary tables for the total final standing volume for each township-matrix combination at the end of the simulation period.
#'
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
#' final_standing_summary <- generate_final_standing_summary(aac_results)
#' print(final_standing_summary)
#' }
#' @import dplyr
#' @export

generate_final_standing_summary <- function(aac_results) {
  final_year <- max(aac_results$Year)

  final_standing <- aac_results %>%
    dplyr::filter(Year == final_year) %>%
    dplyr::group_by(township, matrix) %>%
    dplyr::summarise(
      final_HW_Volume = mean(HW_Volume, na.rm = TRUE),
      final_SW_Volume = mean(SW_Volume, na.rm = TRUE),
      final_Total_Volume = mean(Total_Volume, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(final_standing)
}
