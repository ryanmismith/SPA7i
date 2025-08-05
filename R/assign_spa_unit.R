#' Assign SPA7i unit based on township codes
#'
#' @description
#' `assign_spa_unit()` maps a character vector of township identifiers to their
#' corresponding SPA7i unit codes (“AE”, “AW”, “RY”, or “SP”), using a predefined
#' lookup list.  Any township not found in those lists returns `NA`.
#'
#' @param township A character vector of township codes (e.g. “T10R9”, “Davis”, etc.).
#'
#' @return A character vector the same length as `township`, with values “AE”, “AW”,
#' “RY”, “SP”, or `NA` for unknown codes.
#'
#' @family UtilityFunctions
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#'   codes <- c("T10R9","Davis","T7R14","BogusTown","T16R13")
#'   assign_spa_unit(codes)
#'   #> [1] "AE"  "RY"  "AW"  NA    "SP"
#' }
#' @export
#'
assign_spa_unit <- function(township) {
  AE_twp <- c(
    "Chapman","Perham","T10R10","T10R6","T10R7","T10R8","T10R9",
    "T11R4","T11R7","T11R8","T11R9","T12R6","T13R5","T13R8",
    "T14R5","T8R10","T8R7","T9R5","T9R7","T9R9","TDR2"
  )
  AW_twp <- c(
    "T5R15","T6R15","T7R13","T7R14","T7R15",
    "T8R11","T8R12","T8R13","T8R14","T9R11","T9R13"
  )
  RY_twp <- c(
    "CSurp","CTown","Davis","LCup","Lincoln",
    "Magal","Oxbow","Rich","UCup","Upton"
  )
  SP_twp <- c(
    "T10R15","T10R16","T11R15","T11R16","T12R15",
    "T13R15","T15R13","T15R15","T16R12","T16R13","T16R14"
  )

  dplyr::case_when(
    township %in% AE_twp ~ "AE",
    township %in% AW_twp ~ "AW",
    township %in% RY_twp ~ "RY",
    township %in% SP_twp ~ "SP",
    TRUE                 ~ NA_character_
  )
}
