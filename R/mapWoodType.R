#' Map Product to Wood Type
#'
#' @description This function maps product names to their respective wood types, either "HW" for hardwood or "SW" for softwood.
#' Any product that is not recognized is mapped to "Other".
#'
#' @details
#' This function is designed to efficiently assign wood types based on a product list.
#' It uses predefined mappings for known products exported from AGM. If a product is not found in the
#' predefined list, it defaults to "Other".
#'
#' The following product mappings are used:
#' \itemize{
#'   \item "ASl" and "ASp" - HW (Hardwood)
#'   \item "BFl" and "BFp" - SW (Softwood)
#'   \item "CEl" and "CEp" - SW
#'   \item "HVl", "HVp", "HVt" - HW
#'   \item "LVl", "LVp", "LVt" - HW
#'   \item "OSl" and "OSp" - SW
#'   \item "SPl" and "SPp" - SW
#'   \item "WPl" and "WPp" - SW
#' }
#'
#' @param product A character vector containing product names.
#'
#' @family InputTransform Functions
#'
#' @return A character vector of the same length as `product` with the wood type ("HW", "SW", or "Other").
#' @importFrom dplyr if_else
#' @examples
#' products <- c("ASl", "BFl", "HVl", "unknown")
#' mapWoodType(products)
#'
#' @export

mapWoodType <- function(product) {
  # Define the mapping in a named vector
  wood_type_mapping <- c(
    "ASl" = "HW", "ASp" = "HW", "BFl" = "SW", "BFp" = "SW",
    "CEl" = "SW", "CEp" = "SW", "HVl" = "HW", "HVp" = "HW",
    "HVt" = "HW", "LVl" = "HW", "LVp" = "HW", "LVt" = "HW",
    "OSl" = "SW", "OSp" = "SW", "SPl" = "SW", "SPp" = "SW",
    "WPl" = "SW", "WPp" = "SW"
  )

  # Map product to wood type using the named vector
  wood_type <- wood_type_mapping[product]

  # Replace any NA values (in case there are unmapped products) with "Other"
  wood_type[is.na(wood_type)] <- "Other"

  return(wood_type)
}
