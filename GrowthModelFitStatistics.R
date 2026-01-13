

# Data --------------------------------------------------------------------

library(tidyverse)
library(broom)

setwd("~/R/SPA7i")

# unified_calibrate_growth.R

# unified_calibrate_growth.R
# End-to-end calibration + penalized (L-BFGS-B) + Bayesian priors in brms.

# unified_calibrate_growth.R
# End-to-end: data prep → global fit → unit fits (L-BFGS-B with shrinkage) → brms priors.

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(brms)
})

# ------------------------------------------------------------------------------
# 0) Working directory (edit if needed)
# setwd("~/R/SPA7i")

# ------------------------------------------------------------------------------
# 1) Helpers
# ------------------------------------------------------------------------------

# Biological max volume depends on HW share (favoring SW-dominant stands)
calculate_max_volume <- function(hw_volume, sw_volume, maxvol = 38) {
  total <- hw_volume + sw_volume
  ratio_hw <- ifelse(total == 0, 0, hw_volume / total)
  round(maxvol * (1 + (1 - ratio_hw) * 0.2), 2)
}

# Map “wood class” → fixed HW share used ONLY to build decline index
ratio_hw_by_wood <- function(wood) {
  # HW class (H, HS) ~ 85% HW; SW class (S, SH, OS, C) ~ 15% HW
  ifelse(wood == "HW", 0.85, 0.15)
}

# Decline index ∈ [0,1] given total_vol and wood class
compute_decline <- function(total_vol, wood, base_vol = 6, maxvol = 38) {
  rhw <- ratio_hw_by_wood(wood)
  hwv <- total_vol * rhw
  swv <- total_vol - hwv
  vmax <- calculate_max_volume(hwv, swv, maxvol = maxvol)
  pmax(0, (total_vol - base_vol) / (vmax - base_vol))
}

# Power-law prediction at row level
pred_power <- function(dec, wood, p, minrate, sHW, sSW) {
  sr <- ifelse(wood == "HW", sHW, sSW)
  pmax(minrate, sr - (sr - minrate) * (dec^p))
}

# Robust Huber loss (k controls outlier influence)
huber_loss <- function(resid, k = 0.15) {
  a <- abs(resid)
  ifelse(a <= k, 0.5 * resid^2, k * (a - 0.5 * k))
}

# ------------------------------------------------------------------------------
# 2) Load & reshape data, label wood class
# ------------------------------------------------------------------------------

# SP hardwoods (H, HS → HW)
sp_hw_long <- read_csv("SPHWGrowOnlyVols.csv",
                       col_types = cols(.default = col_double(),
                                        `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`) %>%
  pivot_longer(-year, names_to = "stand", values_to = "volume") %>%
  filter(!is.na(volume)) %>%
  mutate(unit = "SP",
         wood = case_when(
           str_starts(stand, "HS") ~ "HW",
           str_starts(stand, "H")  ~ "HW",
           TRUE ~ NA_character_
         ))

# SP softwoods (S, SH, C → SW)
sp_sw_long <- read_csv("SPSWGrowOnlyVols.csv",
                       col_types = cols(.default = col_double(),
                                        `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`) %>%
  pivot_longer(-year, names_to = "stand", values_to = "volume") %>%
  filter(!is.na(volume)) %>%
  mutate(unit = "SP",
         wood = case_when(
           str_starts(stand, "SH") ~ "SW",
           str_starts(stand, "S")  ~ "SW",
           str_starts(stand, "C")  ~ "SW",
           TRUE ~ NA_character_
         ))

# RY mixed (OS, S, SH, C → SW; H, HS → HW)
ry_long <- read_csv("RY_GrowOnly.csv",
                    col_types = cols(.default = col_double(),
                                     `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`) %>%
  pivot_longer(-year, names_to = "stand", values_to = "volume") %>%
  filter(!is.na(volume)) %>%
  mutate(unit = "RY",
         wood = case_when(
           str_starts(stand, "OS") ~ "SW",
           str_starts(stand, "SH") ~ "SW",
           str_starts(stand, "S")  ~ "SW",
           str_starts(stand, "C")  ~ "SW",
           str_starts(stand, "HS") ~ "HW",
           str_starts(stand, "H")  ~ "HW",
           TRUE ~ NA_character_
         ))

# Combine
long_df <- bind_rows(sp_hw_long, sp_sw_long, ry_long) %>%
  filter(!is.na(wood)) %>%
  mutate(wood = factor(wood, levels = c("HW", "SW")))

# ------------------------------------------------------------------------------
# 3) Annualized growth, decline index, and filters
# ------------------------------------------------------------------------------

base_vol <- 6
maxvol   <- 38

long_df <- long_df %>%
  arrange(unit, stand, year) %>%
  group_by(unit, stand) %>%
  mutate(
    total_vol = volume,
    growth    = volume - lag(volume),
    interval  = 5,
    growth_yr = growth / interval
  ) %>%
  ungroup() %>%
  filter(!is.na(growth_yr),
         is.finite(growth_yr),
         interval > 0) %>%
  filter(total_vol >= 2, total_vol <= 45,   # requested volume window
         growth_yr >= 0.01, growth_yr < .95) %>% # drop negatives and big outliers
  mutate(
    decline_raw = compute_decline(total_vol, wood, base_vol, maxvol),
    isHW        = as.numeric(wood == "HW")
  )


# model fit ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(moments)   # skewness
library(tibble)
library(SPA7i)

# --- Keys should define a STAND-YEAR ---
id_vars <- c("unit", "stand", "year")

# Aggregate to stand-year totals (HW, SW, observed increment)
stand_year <- long_df %>%
  filter(unit %in% c("SP","RY")) %>%
  group_by(across(all_of(id_vars))) %>%
  summarise(
    hw_volume = sum(if_else(wood == "HW", total_vol, 0), na.rm = TRUE),
    sw_volume = sum(if_else(wood == "SW", total_vol, 0), na.rm = TRUE),
    obs_total = sum(growth_yr, na.rm = TRUE),   # cords/ac/yr
    .groups = "drop"
  ) %>%
  mutate(total_volume = hw_volume + sw_volume)

# --- Params: untempered (original) and tempered (a * 0.9) ---
params_untempered <- tibble::tribble(
  ~unit, ~wood, ~a,    ~mu,   ~sigma,
  "RY",  "HW",  0.532, 28.3,  22.6,
  "RY",  "SW",  0.620, 29.4,  20.4,
  "SP",  "HW",  0.537, 32.0,  24.5,
  "SP",  "SW",  0.580, 26.5,  21.5
)

gaussian <- function(vol, a, mu, sigma) {
  a * exp(-0.5 * ((vol - mu) / sigma)^2)
}

params_wide <- params_untempered %>%
  tidyr::pivot_wider(
    names_from = wood,
    values_from = c(a, mu, sigma),
    names_glue = "{.value}_{wood}"
  ) %>%
  mutate(
    a_HW_t = 0.9 * a_HW,
    a_SW_t = 0.9 * a_SW
  )

# --- Join params to stand-year ---
stand_year <- stand_year %>%
  left_join(params_wide, by = "unit")

# --- Predicted rates (cords/ac/yr); combine as weighted average by species share ---
stand_year <- stand_year %>%
  mutate(
    share_hw = if_else(total_volume > 0, hw_volume / total_volume, 0),
    share_sw = 1 - share_hw,

    # Species-specific Gaussian rates
    G_HW_untemp = gaussian(hw_volume, a_HW,    mu_HW,    sigma_HW),
    G_SW_untemp = gaussian(sw_volume, a_SW,    mu_SW,    sigma_SW),
    G_HW_temp   = gaussian(hw_volume, a_HW_t,  mu_HW,    sigma_HW),
    G_SW_temp   = gaussian(sw_volume, a_SW_t,  mu_SW,    sigma_SW),

    # Weighted-average total growth rate
    pred_untempered = share_hw * G_HW_untemp + share_sw * G_SW_untemp,
    pred_tempered   = share_hw * G_HW_temp   + share_sw * G_SW_temp
  ) %>%
  select(-starts_with("a_"), -starts_with("mu_"), -starts_with("sigma_"))

# --- Optional: SPA7i functions for comparison (rates) ---
stand_year <- stand_year %>%
  mutate(
    pred_gaussian_spa = pmap_dbl(list(unit, hw_volume, sw_volume),
                                 ~ calculate_growth_rate(township = ..1, hw_volume = ..2, sw_volume = ..3)),
    pred_power_spa    = pmap_dbl(list(unit, hw_volume, sw_volume),
                                 ~ calculate_growth_rate_power(township = ..1, hw_volume = ..2, sw_volume = ..3))
  )

# --- Residuals (pred - obs): positive = overprediction ---
residuals_long <- stand_year %>%
  transmute(
    !!!syms(id_vars),
    obs_total,
    resid_untempered = pred_untempered   - obs_total,
    resid_tempered   = pred_tempered     - obs_total,
    resid_gaussian   = pred_gaussian_spa - obs_total,
    resid_power      = pred_power_spa    - obs_total
  ) %>%
  pivot_longer(starts_with("resid_"),
               names_to = "model", names_prefix = "resid_",
               values_to = "residual") %>%
  mutate(model = recode(model,
                        untempered = "Gaussian (untempered, params)",
                        tempered   = "Gaussian (tempered, a×0.9)",
                        gaussian   = "Gaussian (SPA7i)",
                        power      = "Power (SPA7i)"))

# --- Diagnostics (overall) ---
model_diagnostics <- residuals_long %>%
  group_by(model) %>%
  summarise(
    n        = dplyr::n(),
    RMSE     = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE      = mean(abs(residual), na.rm = TRUE),
    Bias     = mean(residual, na.rm = TRUE),
    PctBias  = Bias / mean(obs_total[!is.na(residual)], na.rm = TRUE),
    MedianE  = median(residual, na.rm = TRUE),
    P90AbsE  = quantile(abs(residual), 0.90, na.rm = TRUE),
    Skewness = moments::skewness(residual, na.rm = TRUE),
    .groups  = "drop"
  )
print(model_diagnostics)

# ---- Volume classes (one scheme) ----
stand_year <- stand_year %>%
  mutate(
    total_volume = hw_volume + sw_volume,
    volume_class = cut(
      total_volume,
      breaks = c(-Inf, 15, 28, Inf),
      labels = c("<15", "15–28", ">28"),
      right = TRUE, include.lowest = TRUE
    )
  )

# Join class + obs back to residuals
residuals_by_class <- residuals_long %>%
  left_join(stand_year %>% select(all_of(id_vars), volume_class),
            by = id_vars, obs_total)

# ---- By-class diagnostics (full) ----
model_diagnostics_by_class <- residuals_by_class %>%
  group_by(model, volume_class) %>%
  summarise(
    n        = dplyr::n(),
    RMSE     = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE      = mean(abs(residual), na.rm = TRUE),
    Bias     = mean(residual, na.rm = TRUE),  # pred - obs; + = overprediction
    PctBias  = Bias / mean(obs_total[!is.na(residual)], na.rm = TRUE),
    Skew     = moments::skewness(residual, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    # Match your display names
    Model = dplyr::recode(model,
                          "Gaussian (tempered, a×0.9)"    = "Gaussian (final)",
                          "Gaussian (untempered, params)" = "Untempered",
                          "Power (SPA7i)"                 = "Power",
                          .default                        = model
    ),
    Volume_Class = as.character(volume_class)
  )

# ---- Minimal table to mirror your prior tribble ----
diagnostics_by_class <- model_diagnostics_by_class %>%
  transmute(
    Model        = Model,
    Volume_Class = Volume_Class,
    RMSE         = round(RMSE, 3),
    MAE          = round(MAE, 3),
    Bias         = round(Bias, 3),
    Skew         = round(Skew, 4)
  ) %>%
  arrange(Model, factor(Volume_Class, levels = c("<15","15–28",">28")))

print(diagnostics_by_class)
