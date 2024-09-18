# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(SPA7i)
library(ggplot2)

# Load and clean the data
vols <- read.csv("~/Projects/SPA/Arc Outputs/SPA_test3.csv")
colnames(vols) <- tolower(colnames(vols))
vols <- vols %>%
  rename(matrix = 'lumpmatrix')

# Calculate HW/SW ratios
HW_SW_Ratios <- vols %>%
  filter(product %in% c("perAcreHW", "perAcreSW")) %>%
  group_by(township, matrix) %>%
  summarise(
    HWRatio = sum(ifelse(product == "perAcreHW", cords, 0)) / sum(cords),
    SWRatio = sum(ifelse(product == "perAcreSW", cords, 0)) / sum(cords),
    perAcreHW = sum(ifelse(product == "perAcreHW", cords, 0)),
    perAcreSW = sum(ifelse(product == "perAcreSW", cords, 0)),
    TotalVolume = sum(cords)
  )

acres <- vols %>% select(township, matrix, product, acres)
ratios <- calculateProductRatios(vols)
ratios <- left_join(ratios, acres)

vols <- vols |> filter(product %in% c('perAcreHW', 'perAcreSW'))
vols_wide <- vols |>
  pivot_wider(
    names_from = product,   # Column to create new columns from
    values_from = cords     # Column containing the values for the new columns
  )

# Run AAC simulation for each matrix and township
run_aac_simulation <- function(hw_volume, sw_volume, aac_percentage, min_stocking,
                               max_harvest, min_aac = TRUE, years = 15) {
  results <- data.frame()
  for (year in 1:years) {
    aac_result <- calculate_aac(hw_volume, sw_volume, aac_percentage, min_stocking, max_harvest, min_aac)
    hw_volume <- max(0, hw_volume + aac_result$hw_growth - aac_result$hw_aac)
    sw_volume <- max(0, sw_volume + aac_result$sw_growth - aac_result$sw_aac)
    results <- rbind(results, data.frame(
      Year = year, HW_Volume = hw_volume, SW_Volume = sw_volume,
      Total_Volume = hw_volume + sw_volume, HW_AAC = aac_result$hw_aac,
      SW_AAC = aac_result$sw_aac, Total_AAC = aac_result$hw_aac + aac_result$sw_aac,
      total_growth = aac_result$growth_rate, hw_growth = aac_result$hw_growth,
      sw_growth = aac_result$sw_growth))
  }
  return(results)
}

run_aac_for_all <- function(df, years = 20) {
  df %>%
    group_by(township, matrix) %>%
    group_map(~ {
      hw_volume <- .x$perAcreHW
      sw_volume <- .x$perAcreSW
      result <- run_aac_simulation(hw_volume, sw_volume, 1, 12, TRUE, years = years)
      result$township <- unique(.x$township)
      result$matrix <- unique(.x$matrix)
      return(result)
    }, .keep = TRUE) %>%
    bind_rows()
}

# Running AAC simulation and joining with ratios
results_combined <- run_aac_for_all(vols_wide)

# Calculating harvest and standing volumes
calculate_product_volumes <- function(aac_results, ratios_df, years = 20) {
  final_results <- data.frame()
  for (year in 1:years) {
    year_aac <- aac_results %>% filter(Year == year) %>%
      select(township, matrix, HW_AAC, SW_AAC, HW_Volume, SW_Volume)
    merged_df <- ratios_df %>%
      left_join(year_aac, by = c("township", "matrix")) %>%
      mutate(
        total_HW_AAC = HW_AAC * acres, total_SW_AAC = SW_AAC * acres,
        product_harvest_volume = ifelse(wood_type == "HW", HWratio * total_HW_AAC, SWratio * total_SW_AAC),
        product_standing_volume = ifelse(wood_type == "HW", HWratio * HW_Volume * acres, SWratio * SW_Volume * acres)
      )
    merged_df$Year <- year
    final_results <- bind_rows(final_results, merged_df %>%
                                 select(township, matrix, product, wood_type, acres, Year,
                                        product_harvest_volume, product_standing_volume))
  }
  return(final_results)
}

product_volumes <- calculate_product_volumes(results_combined, ratios)

# Filtering and calculating product values
product_values <- c("ASl" = 120, "ASp" = 115, "BFl" = 185, "BFp" = 20, "CEl" = 250,
                    "CEp" = 0, "HVl" = 300, "HVp" = 25, "HVt" = 135, "LVl" = 140,
                    "LVp" = 25, "LVt" = 95, "OSl" = 75, "OSp" = 20, "SPl" = 165,
                    "SPp" = 20, "WPl" = 150, "WPp" = 10)

product_values_df <- data.frame(product = names(product_values), product_value = as.numeric(product_values))

product_volumes <- product_volumes %>%
  left_join(product_values_df, by = "product") %>%
  mutate(
    harvest_value = product_harvest_volume * product_value,
    standing_value = product_standing_volume * product_value
  )

product_volumes <- product_volumes |> filter(is.na(harvest_value) == FALSE)

# Calculate NPV values
NPVvalues <- product_volumes %>%
  group_by(Year) %>%
  summarise(harvest_value = sum(harvest_value), standing_value = sum(standing_value))


max_year <- max(NPVvalues$Year)
final_standing_value <- NPVvalues %>% filter(Year == max_year) %>% pull(standing_value)

# Creating the final flow dataframe
flow_df <- NPVvalues %>%
  select(Year, harvest_value)

# Monte Carlo Analysis
npv <- monteCarloAnalysis(Flow = flow_df$harvest_value, Occurrence = flow_df$Year,
                          NominalRate = .05, TerminalYear = 20, FutureValue = final_standing_value,
                          Exit = FALSE)

# Plot NPV distribution with confidence intervals
mean_NPV <- mean(npv$NPVs)
CI_80 <- npv$CI_80

ggplot(data.frame(NPV = npv$NPVs), aes(x = NPV)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_NPV), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = CI_80[1]), color = "orange", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = CI_80[2]), color = "orange", linetype = "dashed", size = 1) +
  annotate("text", x = mean_NPV + 0.2 * mean_NPV, y = Inf, label = paste0("Mean: $", scales::comma(round(mean_NPV, 2))),
           color = "black", size = 6, vjust = 1.5, fontface = "bold") +
  scale_x_continuous(breaks = seq(0, max(npv$NPVs), by = 50000), labels = scales::comma) +
  labs(title = "NPV Distribution - Holding the Property", x = "NPV", y = "Frequency",
       caption = "Green dashed line: Mean NPV, Yellow dashed lines: 80% Confidence Intervals") +
  theme_minimal()

