#' Calculate Product Ratios by Wood Type, Township, and Matrix
#'
#' @description This function calculates the percentage of each product within its wood type (HW or SW) for each combination of township and matrix. The function uses the `mapWoodType` function to categorize the products.
#'
#' @param df A dataframe containing four columns: 'township', 'matrix', 'product', and 'cords'.
#'
#' @family InputTransform Functions
#'
#' @return A dataframe with the columns: 'township', 'matrix', 'product', 'woodtype', 'cords', 'HWratio', and 'SWratio'. For HW products, the SWratio will be 0 and vice versa.
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @examples
#' df <- data.frame(
#'   township = c("T1", "T1", "T1", "T2", "T2"),
#'   matrix = c("M1", "M1", "M1", "M2", "M2"),
#'   product = c("ASl", "LVl", "CEl", "BFp", "HVp"),
#'   cords = c(10, 15, 20, 5, 30)
#' )
#'
#' calculateProductRatios(df)
#'
#' @export

calculateProductRatios <- function(df) {

  # Apply the mapWoodType function to categorize products as HW or SW
  df$wood_type <- mapWoodType(df$product)

  # Group by township, matrix, and wood_type, and calculate the total cords for HW and SW separately
  total_cords_by_type <- df |>
    dplyr::group_by(.data$township, .data$matrix, .data$wood_type) |>
    dplyr::summarize(total_cords_by_type = sum(.data$cords), .groups = "drop")

  df <- df |>
    dplyr::left_join(total_cords_by_type, by = c("township", "matrix", "wood_type"))

  df <- df |>
    dplyr::mutate(
      HWratio = ifelse(.data$wood_type == "HW", .data$cords / .data$total_cords_by_type, 0),
      SWratio = ifelse(.data$wood_type == "SW", .data$cords / .data$total_cords_by_type, 0)
    )

  result <- df |>
    dplyr::select(.data$township, .data$matrix, .data$product,
                  .data$wood_type, .data$cords, .data$HWratio, .data$SWratio)

  return(result)
}
