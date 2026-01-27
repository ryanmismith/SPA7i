#' Run System-Level AAC Simulation with Global Constraints
#'
#' @description A vectorized, system-level simulation that evolves all stands simultaneously year-by-year.
#' Unlike \code{\link{run_aac_for_all}}, which processes stands in isolation, this function allows for
#' global constraints (such as \code{max_harvest_acres}) that apply across the entire ownership.
#' It includes a Year 0 initialization state to match legacy reporting formats.
#'
#' Handles "Oversized Stands" by splitting them into smaller pieces before simulation to ensure
#' they can fit into the annual \code{max_harvest_acres} budget. If a matrix is >50\% the size of
#' \code{max_harvest_acres} it is automatically split in n parts where
#' n = \code{matrix_acres * 3 / max_harvest_acres}.
#' Automatically aggregates results back to the original Matrix level after simulation.
#'
#' @details
#' \strong{System-Level vs. Stand-Level}
#'
#' This function iterates through time (Year 1, then Year 2...) rather than through stands. This allows the model
#' to track the cumulative acres harvested across the entire landscape in a single year and stop harvesting
#' once a global limit is reached.
#'
#' \strong{Harvest Modes & Logic}
#'
#' The function supports two distinct harvesting behaviors controlled by \code{harvest_mode}:
#'
#' \itemize{
#'   \item \strong{1. Percentage Mode (\code{harvest_mode = "percentage"}):}
#'   \itemize{
#'     \item \strong{Trigger:} Harvest occurs if the stand volume is above \code{min_stocking}.
#'     \item \strong{Amount:} The model harvests a percentage (\code{aac_percentage}) of the stand's \strong{growth} for that year.
#'     \item \strong{Concept:} This mimics a "skimming" approach where capital is preserved and only interest (growth) is removed.
#'   }
#'   \item \strong{2. Concentrated Mode (\code{harvest_mode = "concentrated"}):}
#'   \itemize{
#'     \item \strong{Trigger:} Harvest occurs ONLY if the stand contains enough "excess" wood to justify an entry.
#'     Specifically: \code{(Total Volume - Min Stocking) >= removal}.
#'     Therefore, a stand is only eligible if its Total Volume is at least \code{min_stocking + removal}.
#'     \item \strong{Amount:} The model removes exactly the \code{removal} amount (e.g., 10 cords/acre).
#'     \item \strong{Concept:} This mimics operational reality where equipment mobilization requires a minimum cut per acre.
#'     If a stand is eligible, it is harvested; otherwise, it is left to grow until it reaches the threshold.
#'   }
#' }
#'
#' \strong{Global Acre Constraints}
#'
#' When \code{max_harvest_acres} is set (i.e., not infinite), the simulation enforces a strict annual budget:
#' \enumerate{
#'   \item At the start of each year, the order of stands is \strong{randomized} (shuffled).
#'   \item The model evaluates stands one by one.
#'   \item If a stand triggers a harvest (based on the logic above), the model checks if adding this stand's acres
#'   would exceed the \code{max_harvest_acres} for the year.
#'   \item \strong{All-or-Nothing Rule:} If the stand fits in the remaining budget, it is harvested. If the stand is larger
#'   than the remaining budget, it is \strong{skipped entirely} for that year (it is not partially harvested).
#'   \item Skipped stands continue to grow and are eligible for the "lottery" again in the next year.
#' }
#'
#' @param df A dataframe containing the forest inventory. Must contain columns: 'township', 'matrix', 'perAcreHW', 'perAcreSW', and \strong{'Acres'}.
#' @param aac_percentage Numeric. Used only in "percentage" mode. The \% of growth to harvest (e.g., 1.0 for 100\% of growth).
#' @param min_stocking Numeric. The baseline volume (cords/acre) that must be maintained. Harvests cannot reduce volume below this floor.
#' @param max_harvest_acres Numeric. The maximum allowable harvest footprint per year across the entire dataset. Defaults to \code{Inf} (unconstrained).
#' @param max_harvest Logical. If TRUE, forces a "reset" harvest in Year 1 to bring stands down to \code{min_stocking}. Defaults to \code{FALSE}.
#' @param min_aac Logical. If TRUE, applies a minimum harvest floor (0.5 cords) if growth rates are very low but stocking is high. Defaults to \code{TRUE}.
#' @param years Integer. Number of years to simulate. Defaults to 20.
#' @param harvest_mode Character. One of "percentage" (skim growth) or "concentrated" (take fixed volume). Defaults to "percentage".
#' @param removal Numeric. Used only in "concentrated" mode. The specific volume (cords/acre) to harvest during an entry.
#' Also acts as the threshold trigger (Stand must have \code{min_stocking + removal} volume to be entered).
#'
#' @return A dataframe containing annual states for every stand, including:
#' \itemize{
#'   \item \code{Year}: 0 to \code{years}.
#'   \item \code{HW_AAC} / \code{SW_AAC}: Volume harvested that year.
#'   \item \code{Entry_Status}: 1 if the stand was entered, 0 otherwise.
#'   \item \code{Harvested_Acres}: The acres harvested (0 if no entry).
#'   \item \code{hw_growth} / \code{sw_growth}: The biological growth calculated for that year.
#' }
#'
#' @family AAC Functions
#' @seealso \code{\link{calculate_aac}}
#'
#' @examples
#' # Prepare dummy data with Acres
#' inventory_df <- data.frame(
#'   township = c("T1", "T1", "T2"),
#'   matrix = c("M1", "M2", "M1"),
#'   perAcreHW = c(15, 10, 20),
#'   perAcreSW = c(15, 20, 5),
#'   Acres = c(100, 50, 200) # Required column
#' )
#'
#' # Scenario A: Concentrated Harvest with Acre Cap
#' # Harvest 10 cords/acre only if volume > (12 + 10).
#' # Limit total harvest to 120 acres/year.
#' result_conc <- run_system_simulation(
#'   df = inventory_df,
#'   aac_percentage = 1.0,    # Ignored in concentrated mode
#'   min_stocking = 12,
#'   max_harvest_acres = 120, # Only T1-M1 (100ac) or T1-M2 (50ac) can happen, not T2-M1 (200ac)
#'   harvest_mode = "concentrated",
#'   removal = 10,
#'   years = 20
#' )
#'
#' # Scenario B: Percentage of Growth (Traditional AAC)
#' result_pct <- run_system_simulation(
#'   df = inventory_df,
#'   aac_percentage = 0.8,
#'   min_stocking = 12,
#'   harvest_mode = "percentage",
#'   years = 20
#' )
#'
#' @export

run_system_simulation <- function(df,
                                  aac_percentage,
                                  min_stocking,
                                  max_harvest_acres = Inf,
                                  max_harvest = FALSE,
                                  min_aac = TRUE,
                                  years = 20,
                                  harvest_mode = "percentage",
                                  removal = 10) {

  # 1. Validation
  if(!"Acres" %in% names(df)) stop("Dataframe must have 'Acres' column.")

  # --- STEP 1: SPLIT OVERSIZED STANDS ---
  # We must work with a local copy of the data
  sim_df <- df
  was_split <- FALSE

  # Only split if we are in "concentrated" mode with a finite limit
  # (Percentage mode doesn't skip stands based on size, so splitting isn't strictly necessary there,
  # but we apply logic consistently if a limit exists)
  if (!is.infinite(max_harvest_acres) && max_harvest_acres > 0) {

    # Identify stands larger than the limit
    large_indices <- which(sim_df$Acres > (0.4 * max_harvest_acres))

    if (length(large_indices) > 0) {
      was_split <- TRUE

      # Separate good stands from large stands
      normal_stands <- sim_df[-large_indices, ]
      large_stands  <- sim_df[large_indices, ]

      split_list <- list()

      for (i in 1:nrow(large_stands)) {
        row <- large_stands[i, ]
        original_acres <- row$Acres

        # 1. Determine Ideal Target Size (25% of max budget)
        target_piece_size <- min(max_harvest_acres, max(40, max_harvest_acres * 0.20))

        n_splits_ideal <- ceiling(original_acres / target_piece_size)

        # 2. Apply Hard Floor (40 acres)
        if (original_acres >= 40) {
          max_splits_allowed <- floor(original_acres / 40)
          n_splits <- min(n_splits_ideal, max_splits_allowed)
          # Ensure we have at least 1 piece
          n_splits <- max(1, n_splits)
        } else {
          # Stand is already < 40, cannot split further
          n_splits <- 1
        }

        # If constraints result in no actual splitting, just keep the row
        if(n_splits == 1) {
          split_list[[i]] <- row
          next
        }

        # 3. Perform the Split
        new_acres <- original_acres / n_splits
        expanded <- row[rep(1, n_splits), ]
        expanded$Acres <- new_acres

        # Rename matrix to ensure unique tracking
        expanded$matrix <- paste0(expanded$matrix, "_part", 1:n_splits)

        split_list[[i]] <- expanded
      }

      # Recombine
      sim_df <- dplyr::bind_rows(normal_stands, dplyr::bind_rows(split_list))
    }
  }

  # --- STEP 2: RUN VECTORIZED SIMULATION (On sim_df) ---

  n_stands <- nrow(sim_df)
  total_rows <- n_stands * (years + 1)

  # Initialize State
  current_hw <- as.numeric(sim_df$perAcreHW)
  current_sw <- as.numeric(sim_df$perAcreSW)
  stand_acres <- as.numeric(sim_df$Acres)
  stand_township <- sim_df$township
  stand_matrix <- sim_df$matrix

  # Pre-allocate Vectors
  out_year     <- integer(total_rows)
  out_township <- character(total_rows)
  out_matrix   <- character(total_rows)
  out_acres    <- numeric(total_rows)

  out_hw_vol   <- numeric(total_rows)
  out_sw_vol   <- numeric(total_rows)
  out_tot_vol  <- numeric(total_rows)

  out_hw_aac   <- numeric(total_rows)
  out_sw_aac   <- numeric(total_rows)
  out_tot_aac  <- numeric(total_rows)

  out_growth_r <- numeric(total_rows)
  out_hw_gr    <- numeric(total_rows)
  out_sw_gr    <- numeric(total_rows)

  out_entry    <- integer(total_rows)
  out_h_acres  <- numeric(total_rows)

  # WRITE YEAR 0
  idx_0 <- 1:n_stands
  out_year[idx_0]     <- 0
  out_township[idx_0] <- stand_township
  out_matrix[idx_0]   <- stand_matrix
  out_acres[idx_0]    <- stand_acres
  out_hw_vol[idx_0]   <- current_hw
  out_sw_vol[idx_0]   <- current_sw
  out_tot_vol[idx_0]  <- current_hw + current_sw

  # Fill Y0 NAs
  out_hw_aac[idx_0] <- NA; out_sw_aac[idx_0] <- NA; out_tot_aac[idx_0] <- NA
  out_growth_r[idx_0] <- NA; out_hw_gr[idx_0] <- NA; out_sw_gr[idx_0] <- NA
  out_entry[idx_0] <- 0; out_h_acres[idx_0] <- 0

  # LOOP YEARS
  for (yr in 1:years) {
    stand_indices <- if (!is.infinite(max_harvest_acres)) sample(n_stands) else 1:n_stands
    harvested_acres_this_year <- 0
    apply_max_harvest <- isTRUE(max_harvest) && yr == 1

    for (i in stand_indices) {
      hw <- current_hw[i]
      sw <- current_sw[i]
      acres <- stand_acres[i]

      aac_result <- calculate_aac(
        township = stand_township[i], hw_volume = hw, sw_volume = sw,
        aac_percentage = aac_percentage, min_stocking = min_stocking,
        max_harvest = apply_max_harvest, min_aac = min_aac,
        harvest_mode = harvest_mode, removal = removal
      )

      actual_entry <- aac_result$entry_made

      if (harvest_mode == "concentrated" && actual_entry) {
        # Check budget
        # Note: Since we split large stands, 'acres' is guaranteed to be <= max_harvest_acres
        if ((harvested_acres_this_year + acres) > max_harvest_acres) {
          aac_result$hw_aac <- 0; aac_result$sw_aac <- 0
          aac_result$entry_made <- FALSE; actual_entry <- FALSE
        } else {
          harvested_acres_this_year <- harvested_acres_this_year + acres
        }
      }

      new_hw <- max(0, hw + aac_result$hw_growth - aac_result$hw_aac)
      new_sw <- max(0, sw + aac_result$sw_growth - aac_result$sw_aac)
      current_hw[i] <- new_hw
      current_sw[i] <- new_sw

      idx <- (yr * n_stands) + i
      out_year[idx] <- yr
      out_township[idx] <- stand_township[i]
      out_matrix[idx] <- stand_matrix[i]
      out_acres[idx] <- acres
      out_hw_vol[idx] <- new_hw
      out_sw_vol[idx] <- new_sw
      out_tot_vol[idx] <- new_hw + new_sw
      out_hw_aac[idx] <- aac_result$hw_aac
      out_sw_aac[idx] <- aac_result$sw_aac
      out_tot_aac[idx] <- aac_result$hw_aac + aac_result$sw_aac
      out_growth_r[idx] <- aac_result$growth_rate
      out_hw_gr[idx] <- aac_result$hw_growth
      out_sw_gr[idx] <- aac_result$sw_growth
      out_entry[idx] <- as.integer(actual_entry)
      out_h_acres[idx] <- if(actual_entry) acres else 0
    }
  }

  results_df <- data.frame(
    Year = out_year, HW_Volume = out_hw_vol, SW_Volume = out_sw_vol, Total_Volume = out_tot_vol,
    HW_AAC = out_hw_aac, SW_AAC = out_sw_aac, Total_AAC = out_tot_aac,
    total_growth = out_growth_r, hw_growth = out_hw_gr, sw_growth = out_sw_gr,
    Entry_Status = out_entry, township = out_township, matrix = out_matrix,
    Acres = out_acres, Harvested_Acres = out_h_acres, stringsAsFactors = FALSE
  )

  # --- STEP 3: AGGREGATE SPLITS BACK TO ORIGINAL ---
  if (was_split) {

    # 1. Clean Matrix Name (remove _part1, _part2...)
    results_df$original_matrix <- sub("_part[0-9]+$", "", results_df$matrix)

    # 2. Summarize
    # We weight per-acre values (Volumes, AAC, Growth) by Acres
    aggregated_df <- results_df %>%
      dplyr::group_by(Year, township, original_matrix) %>%
      dplyr::summarise(
        # Sums
        Total_Acres_Sum = sum(Acres),
        Harvested_Acres = sum(Harvested_Acres),

        # Weighted Averages for Per-Acre Values
        HW_Volume = sum(HW_Volume * Acres) / sum(Acres),
        SW_Volume = sum(SW_Volume * Acres) / sum(Acres),
        Total_Volume = sum(Total_Volume * Acres) / sum(Acres),

        HW_AAC = sum(HW_AAC * Acres) / sum(Acres),
        SW_AAC = sum(SW_AAC * Acres) / sum(Acres),
        Total_AAC = sum(Total_AAC * Acres) / sum(Acres),

        total_growth = sum(total_growth * Acres) / sum(Acres),
        hw_growth = sum(hw_growth * Acres) / sum(Acres),
        sw_growth = sum(sw_growth * Acres) / sum(Acres),

        # Entry Status is 1 if ANY part was entered
        Entry_Status = as.integer(sum(Entry_Status) > 0),

        .groups = "drop"
      ) %>%
      dplyr::rename(matrix = original_matrix, Acres = Total_Acres_Sum)

    # Ensure column order matches standard output
    final_cols <- names(results_df)[names(results_df) != "original_matrix"]
    return(aggregated_df[final_cols])

  } else {
    return(results_df)
  }
}
