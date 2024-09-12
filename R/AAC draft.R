# # Updated calculate_aac function
# calculate_aac <- function(hw_volume, sw_volume, aac_percentage, min_volume_for_aac) {
#   total_volume <- hw_volume + sw_volume
#
#   # Check for NA or zero volumes
#   if (is.na(total_volume) || total_volume == 0) {
#     return(list(hw_aac = 0, sw_aac = 0, hw_growth = 0, sw_growth = 0))
#   }
#
#   # Calculate the maximum volume using calculate_max_volume
#   max_volume <- calculate_max_volume(hw_volume, sw_volume)
#
#   # Calculate HW and SW growth rates using calculate_growth_rate function
#   hw_growth_rate <- calculate_growth_rate(total_volume, max_volume, start_rate = 0.40, min_rate = 0.01)
#   sw_growth_rate <- calculate_growth_rate(total_volume, max_volume, start_rate = 0.45, min_rate = 0.02)
#
#   # Calculate the ratio of HW and SW based on the total volume
#   hw_ratio <- if (hw_volume == 0) 0 else hw_volume / total_volume
#   sw_ratio <- if (sw_volume == 0) 0 else sw_volume / total_volume
#
#   # Prorate the growth rates by their respective volume ratios
#   prorated_hw_growth <- hw_growth_rate * hw_ratio
#   prorated_sw_growth <- sw_growth_rate * sw_ratio
#
#   # Total potential growth
#   total_growth <- prorated_hw_growth + prorated_sw_growth
#
#   # Check if the total volume is below the minimum threshold
#   if (total_volume < min_volume_for_aac) {
#     # No AAC applied, just growth
#     hw_aac <- 0
#     sw_aac <- 0
#   } else {
#     # Apply AAC only if the total volume stays above the threshold after cutting
#     if ((total_volume - aac_percentage * total_growth) >= min_volume_for_aac) {
#       hw_aac <- aac_percentage * prorated_hw_growth
#       sw_aac <- aac_percentage * prorated_sw_growth
#     } else {
#       # No AAC if cutting would drop below the threshold
#       hw_aac <- 0
#       sw_aac <- 0
#     }
#   }
#
#   return(list(hw_aac = hw_aac, sw_aac = sw_aac, hw_growth = prorated_hw_growth, sw_growth = prorated_sw_growth))
# }
#
# # Simulate AAC over 30 years
# hw_volume <- 14  # Half of total volume (28)
# sw_volume <- 14  # Half of total volume (28)
# aac_percentage <- 2  # 200% of the growth can be harvested
# min_volume_for_aac <- 28  # Minimum volume before AAC calculation
# years <- 30  # Number of years to run the loop
#
# # Data storage
# results <- data.frame(Year = numeric(), HW_Volume = numeric(), SW_Volume = numeric(), Total_Volume = numeric(), HW_AAC = numeric(), SW_AAC = numeric())
#
# # Loop over 30 years
# for (year in 1:years) {
#   # Calculate AAC for the year
#   aac_result <- calculate_aac(hw_volume, sw_volume, aac_percentage, min_volume_for_aac)
#
#   # Update volumes for next year, ensuring no negative or NaN volumes
#   hw_volume <- hw_volume + aac_result$hw_growth - aac_result$hw_aac
#   sw_volume <- sw_volume + aac_result$sw_growth - aac_result$sw_aac
#
#   # Ensure volumes stay positive
#   hw_volume <- max(0, hw_volume)
#   sw_volume <- max(0, sw_volume)
#
#   # Recalculate total volume
#   total_volume <- hw_volume + sw_volume
#
#   # Store the results
#   results <- rbind(results, data.frame(
#     Year = year,
#     HW_Volume = hw_volume,
#     SW_Volume = sw_volume,
#     Total_Volume = total_volume,
#     HW_AAC = aac_result$hw_aac,
#     SW_AAC = aac_result$sw_aac
#   ))
# }
#
# # Display the results
# print(results)
#
