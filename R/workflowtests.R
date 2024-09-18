vols <- read.csv("~/Projects/SPA/Arc Outputs/SPA_test3.csv")
library(dplyr)
library(SPA7i)
library(tidyr)
library(purrr)


HW_SW <- vols |>
  filter(Product %in% c("perAcreHW", "perAcreSW")) |>
  select(-Acres)

HW_SW_Ratios <- vols |>
  filter(Product %in% c("perAcreHW", "perAcreSW")) |>
  select(Township, LumpMatrix, Product, Cords) |>
  group_by(Township, LumpMatrix) |>
  summarise(
    HWRatio = sum(ifelse(Product == "perAcreHW", Cords, 0)) / sum(Cords),
    SWRatio = sum(ifelse(Product == "perAcreSW", Cords, 0)) / sum(Cords),
    perAcreHW = sum(ifelse(Product == "perAcreHW", Cords, 0)), # Sum of perAcreHW Cords
    perAcreSW = sum(ifelse(Product == "perAcreSW", Cords, 0)), # Sum of perAcreSW Cords
    TotalVolume = sum(Cords) # Total volume for the township and matrix
  )

colnames(vols) <- tolower(colnames(vols))

vols <- vols |> rename(matrix = 'lumpmatrix')

ratios <- calculateProductRatios(vols)

ratios |> group_by(township, matrix) |> summarise(sum(HWratio), sum(SWratio))

vols <- vols |> filter(product %in% c('perAcreHW', 'perAcreSW'))
vols_wide <- vols |>
  pivot_wider(
    names_from = product,   # Column to create new columns from
    values_from = cords     # Column containing the values for the new columns
  )

run_aac_simulation <- function(hw_volume, sw_volume, aac_percentage, min_stocking,
                               max_harvest, min_aac = TRUE, years = 15) {
  results <- data.frame(Year = numeric(), HW_Volume = numeric(), SW_Volume = numeric(),
                        Total_Volume = numeric(), HW_AAC = numeric(), SW_AAC = numeric(),
                        Total_AAC = numeric(), total_growth = numeric(),
                        hw_growth = numeric(), sw_growth = numeric())

  for (year in 1:years) {
    # Calculate AAC for the year
    aac_result <- calculate_aac(hw_volume, sw_volume, aac_percentage, min_stocking,
                                max_harvest, min_aac)

    # Update volumes for next year, ensuring no negative or NaN volumes
    hw_volume <- hw_volume + aac_result$hw_growth - aac_result$hw_aac
    sw_volume <- sw_volume + aac_result$sw_growth - aac_result$sw_aac

    # Ensure volumes stay positive
    hw_volume <- max(0, hw_volume)
    sw_volume <- max(0, sw_volume)

    # Recalculate total volume
    total_volume <- hw_volume + sw_volume

    # Store the results
    results <- rbind(results, data.frame(
      Year = year,
      HW_Volume = hw_volume,
      SW_Volume = sw_volume,
      Total_Volume = total_volume,
      HW_AAC = aac_result$hw_aac,
      SW_AAC = aac_result$sw_aac,
      Total_AAC = aac_result$hw_aac + aac_result$sw_aac,
      total_growth = aac_result$growth_rate,
      hw_growth = aac_result$hw_growth,
      sw_growth = aac_result$sw_growth
    ))
  }
  return(results)
}
run_aac_for_all <- function(df, years = 20) {
  # Group by township and matrix, and apply the simulation for each combination
  result_list <- df %>%
    group_by(township, matrix) %>%
    group_map(~ {
      hw_volume <- .x$perAcreHW
      sw_volume <- .x$perAcreSW

      # Run AAC simulation for each group (township and matrix)
      result <- run_aac_simulation (
        hw_volume = hw_volume,
        sw_volume = sw_volume,
        aac_percentage = 1,  # setting AAC percentage to 1 as requested
        min_stocking = 12,   # using default min_stocking
        max_harvest = TRUE, # Default is FALSE
        years = 20
      )
      # Add township and matrix to the result dataframe
      result$township <- unique(.x$township)
      result$matrix <- unique(.x$matrix)

      return(result)
    }, .keep = TRUE)

  return(result_list)
}

results <- run_aac_for_all(vols_wide)
str(results)
results_combined <- bind_rows(results)

acres <- vols |> select(township, matrix, acres)
ratios <- left_join(ratios, acres)
str(ratios)


# Function to calculate the product volume for each year
# Function to calculate the product volume (harvest) and total standing volume for each year
calculate_product_volumes <- function(aac_results, ratios_df, years = 20) {
  final_results <- data.frame()

  for (year in 1:years) {
    # Extract AAC values and volumes for the current year
    year_aac <- aac_results %>%
      filter(Year == year) %>%
      select(township, matrix, HW_AAC, SW_AAC, HW_Volume, SW_Volume)

    # Merge the AAC values with the product ratios and acres
    merged_df <- ratios_df %>%
      left_join(year_aac, by = c("township", "matrix")) %>%
      mutate(
        # Calculate total AAC for HW and SW per township/matrix based on acres
        total_HW_AAC = HW_AAC * acres,
        total_SW_AAC = SW_AAC * acres,

        # Calculate the harvest volume for each product based on HW or SW AAC
        product_harvest_volume = ifelse(wood_type == "HW",
                                        HWratio * total_HW_AAC,  # HW product harvest volume
                                        SWratio * total_SW_AAC),  # SW product harvest volume

        # Calculate the total standing volume for each product based on HW or SW standing volume
        product_standing_volume = ifelse(wood_type == "HW",
                                         HWratio * HW_Volume * acres,  # HW product standing volume
                                         SWratio * SW_Volume * acres)  # SW product standing volume
      )

    # Add the year information
    merged_df$Year <- year

    # Select the relevant columns (including the new standing volume and harvest volume)
    merged_df <- merged_df %>%
      select(township, matrix, product, wood_type, acres, Year,
             product_harvest_volume, product_standing_volume)

    # Append the result for this year
    final_results <- bind_rows(final_results, merged_df)
  }

  return(final_results)
}

# Assuming you already have the aac_results (from run_aac_for_all_combined function)
# And ratios_df is your dataframe containing product ratios

# Run the function to calculate product volumes and standing volumes for each year
product_volumes <- calculate_product_volumes(results_combined, ratios)
product_volumes <- product_volumes |> filter(product_standing_volume > 0)
# View the final results
print(product_volumes)

x <- product_volumes |> filter(product_harvest_volume  > 0) |>
  group_by(township, matrix, Year) |> summarize(sum(product_volume))
acres |> group_by(matrix) |> summarise(mean(acres))
# A tibble: 5 × 2
# matrix `mean(acres)`
# <chr>          <dbl>
#   1 H1A-N           79.6
# 2 H2A-N            9.3
# 3 H3C-N           16.8
# 4 HS3D-N           8.9
# 5 SH3C-N          10.6

test <- product_volumes |> filter(Year == 19) |>  filter(matrix == 'H1A-N')


product_values <- c(
  "ASl" = 120, "ASp" = 115, "BFl" = 85, "BFp" = 90,
  "CEl" = 105, "CEp" = 110, "HVl" = 130, "HVp" = 125,
  "HVt" = 135, "LVl" = 140, "LVp" = 100, "LVt" = 95,
  "OSl" = 75, "OSp" = 80, "SPl" = 65, "SPp" = 60,
  "WPl" = 50, "WPp" = 55
)

# Print product values
print(product_values)

product_values_df <- data.frame(
  product = names(product_values),
  product_value = as.numeric(product_values)
)

product_volumes <- product_volumes %>%
  left_join(product_values_df, by = "product") %>%
  mutate(
    # Multiply product volumes by the corresponding product value
    harvest_value = product_harvest_volume * product_value,
    standing_value = product_standing_volume * product_value
  )

# Assuming NPVvalues is already calculated as:
NPVvalues <- product_volumes |>
  group_by(Year) |>
  summarise(
    harvest_value = sum(harvest_value),
    standing_value = sum(standing_value)
  )

# Extract the max year and standing_value for the max year
max_year <- max(NPVvalues$Year)
final_standing_value <- NPVvalues %>%
  filter(Year == max_year) %>%
  pull(standing_value)

# Create a flow dataframe with harvest_value for each year
flow_df <- NPVvalues %>%
  select(Year, harvest_value) %>%
  rename(flow = harvest_value)

# View the final flow dataframe
print(flow_df)

npv <- monteCarloAnalysis(Flow = flow_df$flow, Occurrence = flow_df$Year,
                         NominalRate = .05,
                   TerminalYear = 20, FutureValue = final_standing_value,
                   Exit = FALSE)

library(ggplot2)

# Calculate the mean NPV value
mean_NPV <- mean(npv$NPVs)

# Define the 80% Confidence Intervals (assuming npv$CI_80 contains a vector of two values)
CI_80 <- npv$CI_80

# Plot with big ticks, mean line, text annotation, and confidence interval lines
ggplot(data.frame(NPV = npv$NPVs), aes(x = NPV)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +

  # Add a vertical line for the mean NPV
  geom_vline(aes(xintercept = mean_NPV), color = "green", linetype = "dashed", size = 1) +

  # Add horizontal lines for the 80% Confidence Interval
  geom_vline(aes(xintercept = CI_80[1]), color = "orange", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = CI_80[2]), color = "orange", linetype = "dashed", size = 1) +

  # Add text annotation for the mean value (example: 150,000)
  annotate("text", x = mean_NPV + 0.2 * mean_NPV, y = Inf, label = paste0("Mean: $", scales::comma(round(mean_NPV, 2))),
           color = "black", size = 6, vjust = 1.5, fontface = "bold") +

  # Customize the x-axis with big ticks
  scale_x_continuous(breaks = seq(0, max(npv$NPVs), by = 50000), labels = scales::comma) +

  # Add labels and theme
  labs(title = "NPV Distribution - Holding the Property",
       x = "NPV",
       y = "Frequency",
       caption = "Green dashed line: Mean NPV, Yellow dashed lines: 80% Confidence Intervals") +

  # Add custom legend for confidence intervals
  scale_linetype_manual(name = "Legend",
                        values = c("dashed", "dotted"),
                        labels = c("Mean NPV", "80% Confidence Intervals")) +

  theme_minimal()

