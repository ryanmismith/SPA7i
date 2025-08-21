library(dplyr)
library(moments)  # for skewness
library(tidyr)
library(purrr)

# Join fitted parameters (untempered) by unit and wood for the untempered Gaussian
untampered_params <- tibble::tribble(
  ~unit, ~wood, ~a, ~mu, ~sigma,
  "RY", "HW", 0.532, 28.3, 22.6,
  "RY", "SW", 0.620, 29.4, 20.4,
  "SP", "HW", 0.537, 32.0, 24.5,
  "SP", "SW", 0.580, 26.5, 21.5
)

# Gaussian helper
gaussian <- function(vol, a, mu, sigma) {
  a * exp(-0.5 * ((vol - mu) / sigma)^2)
}

long_df_eval <- long_df %>%
  mutate(
    pred_gaussian = mapply(
      calculate_growth_gaussian,
      township = unit,
      hw_volume = hw_volume,
      sw_volume = sw_volume
    )
  )


library(dplyr)
library(tibble)
library(SPA7i)
# Assuming you have the original fitted Gaussian parameters:

original_params <- tibble::tribble(
  ~unit, ~wood, ~a, ~mu, ~sigma,
  "RY", "HW", 0.532, 28.3, 22.6,
  "RY", "SW", 0.620, 29.4, 20.4,
  "SP", "HW", 0.537, 32.0, 24.5,
  "SP", "SW", 0.580, 26.5, 21.5
)
# Gaussian function
gaussian <- function(vol, a, mu, sigma) {
  a * exp(-0.5 * ((vol - mu) / sigma)^2)
}

# Main pipeline
long_df_eval <- long_df %>%
  filter(unit %in% c("SP", "RY")) %>%
  mutate(
    # Estimate missing species volume
    hw_volume = if_else(wood == "HW", volume, 0),
    sw_volume = if_else(wood == "SW", volume, 0),

    # Tempered Gaussian
    pred_gaussian = mapply(
      calculate_growth_rate,
      township = unit,
      hw_volume = hw_volume,
      sw_volume = sw_volume
    ),

    # Power-law
    pred_power = mapply(
      calculate_growth_rate_power,
      township = unit,
      hw_volume = hw_volume,
      sw_volume = sw_volume
    )
  ) %>%
  # Join original Gaussian params and compute untempered predictions
  left_join(original_params, by = c("unit", "wood")) %>%
  mutate(
    pred_untempered = gaussian(volume, a, mu, sigma),

    # Residuals
    resid_gaussian   = growth_yr - pred_gaussian,
    resid_power      = growth_yr - pred_power,
    resid_untempered = growth_yr - pred_untempered
  )



library(tidyr)
library(moments)

residuals_df <- long_df_eval %>%
  pivot_longer(
    cols = starts_with("resid_"),
    names_to = "model",
    names_prefix = "resid_",
    values_to = "residual"
  )

model_diagnostics <- residuals_df %>%
  group_by(model) %>%
  summarise(
    RMSE  = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE   = mean(abs(residual), na.rm = TRUE),
    Skew  = skewness(residual, na.rm = TRUE),
    .groups = "drop"
  )

print(model_diagnostics)

# 1. Add volume class bin
long_df_eval <- long_df_eval %>%
  mutate(
    volume_class = case_when(
      volume < 15       ~ "<15",
      volume <= 28      ~ "15–28",
      volume > 28       ~ ">28"
    )
  )

# 2. Pivot residuals into long format
residuals_df <- long_df_eval %>%
  pivot_longer(
    cols = starts_with("resid_"),
    names_to = "model",
    names_prefix = "resid_",
    values_to = "residual"
  )

# 3. Compute diagnostics by model and volume class
model_diagnostics_by_class <- residuals_df %>%
  group_by(model, volume_class) %>%
  summarise(
    RMSE = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE  = mean(abs(residual), na.rm = TRUE),
    Skew = skewness(residual, na.rm = TRUE),
    .groups = "drop"
  )

# 4. View results
print(model_diagnostics_by_class)



long_df_eval <- long_df_eval %>%
  mutate(
    volume_class = case_when(
      volume < 12       ~ "<12",
      volume <= 24      ~ "12–24",
      volume > 24       ~ ">24"
    )
  )

# 2. Pivot residuals into long format
residuals_df <- long_df_eval %>%
  pivot_longer(
    cols = starts_with("resid_"),
    names_to = "model",
    names_prefix = "resid_",
    values_to = "residual"
  )

# 3. Compute diagnostics by model and volume class
model_diagnostics_by_class <- residuals_df %>%
  group_by(model, volume_class) %>%
  summarise(
    RMSE = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE  = mean(abs(residual), na.rm = TRUE),
    Skew = skewness(residual, na.rm = TRUE),
    .groups = "drop"
  )

# 4. View results
print(model_diagnostics_by_class)

