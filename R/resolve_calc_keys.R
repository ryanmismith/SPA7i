#' Resolve calculation keys with township precedence (vectorized)
#'
#' Decision order per row:
#' 1) If `township` is in one of the allowed lists, use that township for calcs and set `calc_unit`
#'    from the list it belongs to.
#' 2) Else, if provided `unit` is valid (one of "RY","AE","AW","SP"), set
#'    `calc_unit = unit` and `township_for_calc = unit`.
#' 3) Else, default to `calc_unit = "AW"` and `township_for_calc = "AW"`.
#'
#' Original inputs are preserved for reporting (see return columns).
#'
#' @param unit character vector; may be NA/invalid. Expected values among "RY","AE","AW","SP".
#' @param township character vector; original labels (kept for spatial/reporting).
#' @param default_unit single string fallback if no valid township or unit (default "AW").
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{unit_input}: normalized (uppercased) `unit` input.
#'   \item \code{township_input}: original `township` (trimmed) for spatial/reporting.
#'   \item \code{calc_unit}: resolved unit for modeling/valuation.
#'   \item \code{township_for_calc}: resolved township key for modeling/valuation.
#' }
#'
#' @examples
#' # Township recognized (belongs to AW list)
#' resolve_calc_keys("AW", "T9R11")
#'
#' # Township unrecognized; valid unit provided -> fallback to unit
#' resolve_calc_keys("RY", "BogusTown")
#'
#' # Neither township nor unit valid -> AW/AW
#' resolve_calc_keys(NA, "BogusTown")
#'
#' # Township recognized in AE list (calc uses AE + the exact township)
#' resolve_calc_keys(NA, "T10R10")
#'
#' @export
resolve_calc_keys <- function(unit, township, default_unit = "AW") {
  stopifnot(length(unit) == length(township))
  
  # ---- Allowed sets ----
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
  allowed_units <- c("RY","AE","AW","SP")
  
  # ---- Normalize inputs (preserve township case in output; compare with exact strings) ----
  up    <- function(x) ifelse(is.na(x), NA_character_, toupper(trimws(as.character(x))))
  trim  <- function(x) ifelse(is.na(x), NA_character_, trimws(as.character(x)))
  
  unit_in <- up(unit)
  twp_in  <- trim(township)
  
  # ---- Vectorized township -> unit membership ----
  # twp_unit is the unit label implied by township membership, or NA if not allowed
  twp_unit <- ifelse(twp_in %in% AE_twp, "AE",
                     ifelse(twp_in %in% AW_twp, "AW",
                            ifelse(twp_in %in% RY_twp, "RY",
                                   ifelse(twp_in %in% SP_twp, "SP", NA_character_))))
  
  twp_valid  <- !is.na(twp_unit)
  unit_valid <- !is.na(unit_in) & unit_in %in% allowed_units
  
  # ---- Precedence logic ----
  # calc_unit
  calc_unit <- ifelse(twp_valid, twp_unit,
                      ifelse(unit_valid, unit_in, default_unit))
  
  # township_for_calc
  township_for_calc <- ifelse(twp_valid, twp_in,
                              ifelse(unit_valid, calc_unit, default_unit))
  
  # ---- Return (preserve originals for reporting) ----
  tibble::tibble(
    unit_input        = unit_in,
    township_input    = twp_in,
    calc_unit         = calc_unit,
    township_for_calc = township_for_calc
  )
}

