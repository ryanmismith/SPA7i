#' Generate AAC Summary Tables
#'
#' @description This function generates summary tables for the average HW AAC, SW AAC, and Total AAC for each township-matrix combination over the simulation period.
#'
#' @param aac_results A dataframe containing AAC simulation results (output from `run_aac_for_all` function).
#'
#' @return A dataframe summarizing average AAC values for each township-matrix combination.
#' @family Summary Functions
#' @import dplyr
#' @examples
#' # Create a sample dataset that mimics the output from run_aac_for_all
#' aac_results <- data.frame(
#'   township = rep(c("Township1", "Township2", "Township3"), each = 3),
#'   matrix = rep(c("MatrixA", "MatrixB", "MatrixC"), times = 3),
#'   HW_AAC = c(10, 15, 20, 12, 18, 25, 14, 19, 22),
#'   SW_AAC = c(30, 35, 40, 32, 37, 42, 34, 39, 44),
#'   Total_AAC = c(40, 50, 60, 44, 55, 67, 48, 58, 66)
#' )
#'
#' # Generate AAC summary
#' aac_summary <- generate_aac_summary(aac_results)
#'
#' # Print the AAC summary
#' print(aac_summary)
#'
#' # Output should show average HW_AAC, SW_AAC, and Total_AAC by township and matrix
#' # Example:
#' # # A tibble: 9 x 5
#' # township matrix  avg_HW_AAC avg_SW_AAC avg_Total_AAC
#' # <chr>    <chr>        <dbl>       <dbl>         <dbl>
#' # 1 Township1 MatrixA        10          30            40
#' # 2 Township1 MatrixB        15          35            50
#' # 3 Township1 MatrixC        20          40            60
#' # 4 Township2 MatrixA        12          32            44
#' # 5 Township2 MatrixB        18          37            55
#' # 6 Township2 MatrixC        25          42            67
#' # 7 Township3 MatrixA        14          34            48
#' # 8 Township3 MatrixB        19          39            58
#' # 9 Township3 MatrixC        22          44            66
#' @export
generate_aac_summary <- function(aac_results) {
  aac_summary <- aac_results |>
    dplyr::group_by(township, matrix) |>
    dplyr::summarise(
      avg_HW_AAC = mean(HW_AAC, na.rm = TRUE),
      avg_SW_AAC = mean(SW_AAC, na.rm = TRUE),
      avg_Total_AAC = mean(Total_AAC, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  return(aac_summary)
}
