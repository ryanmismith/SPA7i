#' Summarize Product Volumes with Acre Expansion and Growth Allocation (Pattern A, base R only)
#'
#' @param aac_results Data frame with columns: Year, township, matrix, hw_growth, sw_growth
#' @param ratios_df   Ratios input for calculateProductVolumes()
#' @param arcOutput   Data frame with columns: township, matrix, acres
#' @param years       Horizon for calculateProductVolumes()
#' @param first_year  First reporting year (default 2)
#' @param per_acre_product_volumes TRUE if calculateProductVolumes() returns per-acre volumes
#'
#' @return list(annual_volumes, summary_volumes, hardwood_volumes)
#' @export
summarizeProductVolumesExpanded <- function(aac_results,
                                            ratios_df,
                                            arcOutput,
                                            years = 20,
                                            first_year = 2,
                                            per_acre_product_volumes = TRUE) {

  # Helper: coerce to numeric robustly without readr (strip non-numeric characters)
  to_num <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) x <- gsub("[^0-9.+\\-eE]", "", x)
    suppressWarnings(as.numeric(x))
  }

  # 1) Product-level volumes (assumed per-acre unless told otherwise)
  product_volumes <- calculateProductVolumes(aac_results, ratios_df, years = years)

  req_cols <- c("year","township","matrix","product",
                "product_standing_volume","product_harvest_volume")
  missing <- setdiff(req_cols, names(product_volumes))
  if (length(missing)) stop("Missing columns in product_volumes: ", paste(missing, collapse = ", "))

  # Mappings
  wood_type_mapping <- c(
    "ASl"="HW","ASp"="HW","BFl"="SW","BFp"="SW",
    "CEl"="SW","CEp"="SW","HVl"="HW","HVp"="HW",
    "HVt"="HW","LVl"="HW","LVp"="HW","LVt"="HW",
    "OSl"="SW","OSp"="SW","SPl"="SW","SPp"="SW",
    "WPl"="SW","WPp"="SW"
  )
  groupings <- c(
    "ASl"="Poplar","ASp"="Poplar",
    "BFl"="Spruce-Fir","BFp"="Spruce-Fir","SPl"="Spruce-Fir","SPp"="Spruce-Fir",
    "CEl"="Cedar","CEp"="Cedar",
    "WPl"="Pine","WPp"="Pine",
    "OSl"="Other Softwood","OSp"="Other Softwood",
    "HVl"="Hardwood","HVp"="Hardwood","HVt"="Hardwood",
    "LVl"="Hardwood","LVp"="Hardwood","LVt"="Hardwood"
  )

  product_volumes <- product_volumes |>
    dplyr::mutate(
      wood_type     = unname(wood_type_mapping[product]),
      species_group = unname(groupings[product]),
      product_class = dplyr::case_when(
        endsWith(product, "l") | endsWith(product, "t") ~ "sawlog",
        endsWith(product, "p") ~ "pulp",
        TRUE ~ "other"
      ),
      product_standing_volume = to_num(product_standing_volume),
      product_harvest_volume  = to_num(product_harvest_volume)
    ) |>
    dplyr::filter(!is.na(species_group), !is.na(wood_type))

  # 2) Acres by Township/Matrix — keep ONLY arcOutput acres
  acres_tbl <- arcOutput |>
    dplyr::group_by(township, matrix) |>
    dplyr::summarise(acres = mean(acres, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(acres = to_num(acres))

  # Drop any acres in product_volumes, then join arc acres
  product_volumes <- product_volumes |>
    dplyr::select(-dplyr::any_of("acres")) |>
    dplyr::left_join(acres_tbl, by = c("township","matrix"))

  # 3) Expand standing/harvest by acres
  if (per_acre_product_volumes) {
    product_volumes <- product_volumes |>
      dplyr::mutate(
        standing_total = product_standing_volume,
        harvest_total  = product_harvest_volume
      )
  } else {
    product_volumes <- product_volumes |>
      dplyr::mutate(
        standing_total = product_standing_volume,
        harvest_total  = product_harvest_volume
      )
  }

  # 4) Growth per-acre (aac_results) -> expand by acres and allocate within HW/SW by standing share
  growth_tmY <- aac_results |>
    dplyr::group_by(township, matrix, Year) |>
    dplyr::summarise(
      hw_growth_pa = sum(hw_growth, na.rm = TRUE),
      sw_growth_pa = sum(sw_growth, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(acres_tbl, by = c("township","matrix")) |>
    dplyr::mutate(
      hw_growth_tot = to_num(hw_growth_pa) * acres,
      sw_growth_tot = to_num(sw_growth_pa) * acres
    )

  # Standing shares within HW/SW at township × matrix × year (use expanded standing totals)
  shares_tm_y <- product_volumes |>
    dplyr::group_by(township, matrix, year, wood_type, species_group) |>
    dplyr::summarise(standing_tot = sum(standing_total, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::group_by(township, matrix, year, wood_type) |>
    dplyr::mutate(
      wt_total = sum(standing_tot, na.rm = TRUE),
      share_within_wt = dplyr::if_else(wt_total > 0, standing_tot / wt_total, 0)
    ) |>
    dplyr::ungroup()

  allocated_growth_tm_y <- shares_tm_y |>
    dplyr::left_join(growth_tmY, by = c("township" = "township",
                                        "matrix"   = "matrix",
                                        "year"     = "Year")) |>
    dplyr::mutate(
      growth_alloc_tot = dplyr::case_when(
        wood_type == "HW" ~ share_within_wt * to_num(hw_growth_tot),
        wood_type == "SW" ~ share_within_wt * to_num(sw_growth_tot),
        TRUE ~ 0
      )
    ) |>
    dplyr::select(year, species_group, growth_total = growth_alloc_tot)

  # 5) Annual outputs (expanded by acres)
  annual_volumes <- product_volumes |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarise(
      standing_total = sum(standing_total, na.rm = TRUE),
      harvest_total  = sum(harvest_total,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      allocated_growth_tm_y |>
        dplyr::group_by(year, species_group) |>
        dplyr::summarise(growth_total = sum(growth_total, na.rm = TRUE), .groups = "drop"),
      by = c("year","species_group")
    ) |>
    dplyr::mutate(growth_total = dplyr::coalesce(growth_total, 0)) |>
    dplyr::arrange(year, species_group)

  # 6) Hardwood sawlog/pulp (expanded)
  hardwood_volumes <- product_volumes |>
    dplyr::filter(species_group == "Hardwood") |>
    dplyr::group_by(year, species_group, product_class) |>
    dplyr::summarise(
      standing_total = sum(standing_total, na.rm = TRUE),
      harvest_total  = sum(harvest_total,  na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::group_by(year, species_group) |>
    dplyr::mutate(
      total_sawlog_standing = sum(standing_total[product_class == "sawlog"], na.rm = TRUE),
      total_pulp_standing   = sum(standing_total[product_class == "pulp"],   na.rm = TRUE),
      total_sawlog_harvest  = sum(harvest_total [product_class == "sawlog"], na.rm = TRUE),
      total_pulp_harvest    = sum(harvest_total [product_class == "pulp"],   na.rm = TRUE),
      product_percent = dplyr::case_when(
        product_class %in% c("sawlog","pulp") ~ standing_total /
          (total_sawlog_standing + total_pulp_standing),
        TRUE ~ NA_real_
      ),
      harvest_percent = dplyr::case_when(
        product_class %in% c("sawlog","pulp") ~ harvest_total /
          (total_sawlog_harvest + total_pulp_harvest),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(group = "hardwood") |>
    dplyr::select(group, year, product_class, standing_total, harvest_total,
                  product_percent, harvest_percent)

  # 7) Summary (expanded)
  final_year <- product_volumes |>
    dplyr::group_by(year) |>
    dplyr::summarise(tv = sum(standing_total, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(tv > 0) |>
    dplyr::pull(year) |>
    max()

  initial_summary <- annual_volumes |>
    dplyr::filter(year == first_year) |>
    dplyr::rename(
      initial_standing = standing_total,
      initial_harvest  = harvest_total,
      initial_growth   = growth_total
    ) |>
    dplyr::mutate(start_year = first_year) |>
    dplyr::select(species_group, start_year, initial_standing, initial_harvest, initial_growth)

  final_summary <- annual_volumes |>
    dplyr::filter(year == final_year) |>
    dplyr::rename(
      final_standing = standing_total,
      final_harvest  = harvest_total,
      final_growth   = growth_total
    ) |>
    dplyr::mutate(final_year = final_year) |>
    dplyr::select(species_group, final_year, final_standing, final_harvest, final_growth)

  summary_volumes <- dplyr::full_join(initial_summary, final_summary, by = "species_group")

  list(
    annual_volumes  = annual_volumes,
    summary_volumes = summary_volumes,
    hardwood_volumes= hardwood_volumes
  )
}
