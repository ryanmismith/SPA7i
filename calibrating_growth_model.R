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

ggplot(data = long_df, aes(total_vol, growth_yr, color = unit)) +
  geom_point() +
  geom_smooth(method = 'loess')

# Quick sanity check
print(table(long_df$unit, long_df$wood))

# ------------------------------------------------------------------------------
# 4) GLOBAL fit (all data) to get robust initial values
#     Parameters: p, minrate; startrateHW/startrateSW from low-decline medians
# ------------------------------------------------------------------------------

# Low-decline “start rates” by wood class
start_zone <- long_df %>% filter(decline_raw < 0.5)

startrate_HW0 <- start_zone %>%
  filter(wood == "HW") %>%
  summarise(m = median(growth_yr, na.rm = TRUE)) %>%
  pull(m)

startrate_SW0 <- start_zone %>%
  filter(wood == "SW") %>%
  summarise(m = median(growth_yr, na.rm = TRUE)) %>%
  pull(m)

if (is.na(startrate_HW0)) startrate_HW0 <- 0.50
if (is.na(startrate_SW0)) startrate_SW0 <- 0.60

# Global objective (Huber loss + soft priors on p and minrate)
rss_global_pm <- function(par, dat, sHW, sSW,
                          huber_k = 0.15,
                          p_center = 1.5, p_sd = 0.5,
                          m_center = 0.25, m_sd = 0.10,
                          lambda = .75) {
  p       <- par["p"]
  minrate <- par["minrate"]
  pred    <- pred_power(dat$decline_raw, dat$wood, p, minrate, sHW, sSW)
  resid   <- dat$growth_yr - pred
  loss    <- huber_loss(resid, k = huber_k)
  rss     <- sum(loss)
  pen     <- ((p - p_center)/p_sd)^2 + ((minrate - m_center)/m_sd)^2
  rss + lambda * pen
}

set.seed(123)
starts <- tibble(
  p       = runif(20, 0.5, 3.0),
  minrate = runif(20, 0.10, 0.40)
)

fit_list <- purrr::pmap(
  list(starts$p, starts$minrate),
  ~ optim(
    par    = c(p = ..1, minrate = ..2),
    fn     = rss_global_pm,
    dat    = long_df,
    sHW    = startrate_HW0,
    sSW    = startrate_SW0,
    method = "L-BFGS-B",
    lower  = c(p = 0.10, minrate = 0.00),
    upper  = c(p = 5.00, minrate = 0.60)
  )
)

best_ix  <- which.min(map_dbl(fit_list, ~ .x$value))
fit_best <- fit_list[[best_ix]]

glob_par <- c(
  p            = unname(fit_best$par["p"]),
  minrate      = unname(fit_best$par["minrate"]),
  startrateHW  = startrate_HW0,
  startrateSW  = startrate_SW0
)

cat("\nGlobal parameters:\n")
print(glob_par)

# ------------------------------------------------------------------------------
# 5) Unit fits (SP, RY) with shrinkage toward global
# ------------------------------------------------------------------------------

# Prior SDs (weaker shrinkage = larger SDs)
sd_prior <- list(
  p            = 0.60,
  minrate      = 0.15,
  startrateHW  = 0.10,
  startrateSW  = 0.10
)
lambda <- 0.15   # penalty weight; raise to shrink harder to global

# Penalized objective for one unit
obj_unit <- function(par, dat, glob, sd, lambda, huber_k = 0.15) {
  if (nrow(dat) == 0L) return(1e12)
  if (any(!is.finite(dat$growth_yr)) || any(!is.finite(dat$decline_raw))) return(1e12)

  p   <- par["p"]
  mr  <- par["minrate"]
  sH  <- par["startrateHW"]
  sS  <- par["startrateSW"]

  pred <- pred_power(dat$decline_raw, dat$wood, p, mr, sH, sS)
  if (any(!is.finite(pred))) return(1e12)

  rss  <- sum(huber_loss(dat$growth_yr - pred, k = huber_k))

  penG <- ((p  - glob["p"])           / sd$p)^2 +
    ((mr - glob["minrate"])     / sd$minrate)^2 +
    ((sH - glob["startrateHW"]) / sd$startrateHW)^2 +
    ((sS - glob["startrateSW"]) / sd$startrateSW)^2

  rss + lambda * penG
}

# Unit-specific low-decline starts
unit_inits <- start_zone %>%
  group_by(unit, wood) %>%
  summarise(sr = median(growth_yr, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = wood, values_from = sr, names_prefix = "startrate") %>%
  mutate(
    startrateHW = coalesce(startrateHW, glob_par["startrateHW"]),
    startrateSW = coalesce(startrateSW, glob_par["startrateSW"])
  ) %>%
  select(unit, startrateHW, startrateSW)

# Multi-start wrapper per unit
fit_one_unit <- function(u,
                         nstarts    = 15,
                         jitter_sd  = c(p = 0.30, minrate = 0.10,
                                        startrateHW = 0.05, startrateSW = 0.05),
                         lower      = c(p = 0.10, minrate = 0.00,
                                        startrateHW = 0.40, startrateSW = 0.45),
                         upper      = c(p = 5.00, minrate = 0.3,
                                        startrateHW = 0.90, startrateSW = 0.95)) {

  dat_u <- dplyr::filter(long_df, unit == u)
  stopifnot(nrow(dat_u) > 0)

  ui <- dplyr::filter(unit_inits, unit == u)
  base <- c(p           = glob_par["p"],
            minrate     = glob_par["minrate"],
            startrateHW = ui$startrateHW,
            startrateSW = ui$startrateSW)

  # --------------------------------------------------------------------------
  # helper to jitter one start inside [lower, upper]
  # --------------------------------------------------------------------------
  jitter_one <- function() {
    stats::setNames(
      pmin(pmax(
        stats::rnorm(length(base), mean = base, sd = jitter_sd), lower), upper),
      names(base)
    )
  }

  starts <- replicate(nstarts, jitter_one(), simplify = FALSE)

  # --------------------------------------------------------------------------
  # keep only those starts that yield a finite objective
  # --------------------------------------------------------------------------
  starts_good <- purrr::keep(
    starts,
    ~ is.finite(obj_unit(.x, dat_u, glob_par, sd_prior, lambda))
  )

  if (length(starts_good) == 0)
    stop("All starting points produced non-finite objectives for unit ", u)

  fits <- purrr::map(
    starts_good,
    ~ optim(
      par    = .x,
      fn     = obj_unit,
      dat    = dat_u,
      glob   = glob_par,
      sd     = sd_prior,
      lambda = lambda,
      method = "L-BFGS-B",
      lower  = lower,
      upper  = upper
    )
  )

  # keep only successful optimisations
  fits <- purrr::keep(fits, ~ is.finite(.x$value) && is.finite(sum(.x$par)))

  if (length(fits) == 0)
    stop("optim() failed on every good start for unit ", u)

  fits[[ which.min(purrr::map_dbl(fits, "value")) ]]
}

fit_SP <- fit_one_unit("SP", nstarts = 15)
fit_RY <- fit_one_unit("RY", nstarts = 15)

unit_par <- bind_rows(
  tibble(unit = "SP", param = names(fit_SP$par), estimate = as.numeric(fit_SP$par)),
  tibble(unit = "RY", param = names(fit_RY$par), estimate = as.numeric(fit_RY$par))
) %>%
  pivot_wider(names_from = param, values_from = estimate)

cat("\nUnit-level parameters (penalized L-BFGS-B):\n")
print(unit_par)
# Unit-level parameters (penalized L-BFGS-B):
#   > print(unit_par)
# # A tibble: 2 × 5
# unit    p.p minrate.minrate startrateHW startrateSW
# <chr> <dbl>           <dbl>       <dbl>       <dbl>
#  1 SP     1.20             0.3       0.4         0.45
#  2 RY     1.52             0.3       0.442       0.524
# ------------------------------------------------------------------------------
# 6) Diagnostics (MAE / RMSE by unit)
# ------------------------------------------------------------------------------

df_preds <- long_df %>%
  left_join(unit_par, by = "unit") %>%
  mutate(pred = pred_power(decline_raw, wood, p.p, minrate.minrate, startrateHW, startrateSW)) |>
  rename(p = "p.p", minrate = "minrate.minrate")

fit_metrics <- df_preds %>%
  group_by(unit) %>%
  summarise(
    MAE  = mean(abs(growth_yr - pred), na.rm = TRUE),
    RMSE = sqrt(mean((growth_yr - pred)^2, na.rm = TRUE)),
    n    = n(),
    .groups = "drop"
  )

cat("\nFit metrics by unit:\n")
print(fit_metrics)

# > print(fit_metrics)
# # A tibble: 2 × 4
# unit    MAE  RMSE     n
# <chr> <dbl> <dbl> <int>
# 1 RY    0.156 0.191   515
# 2 SP    0.176 0.212   388
library(dplyr)
library(ggplot2)
library(scales)

# Prep: add residuals
dfp <- df_preds %>%
  mutate(resid = growth_yr - pred)


# PLOTS  ------------------------------------------------------------------

# 1) Observed vs Predicted (1:1) — by unit, colored by wood class
p1 <- ggplot(dfp, aes(x = pred, y = growth_yr, color = wood)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_point(alpha = 0.35, size = 1.2) +
  facet_wrap(~ unit, nrow = 1) +
  coord_equal() +
  labs(
    title = "Observed vs Predicted Annual Growth",
    x = "Predicted (cords/acre/yr)",
    y = "Observed (cords/acre/yr)",
    color = "Class"
  ) +
  theme_minimal(base_size = 13)
print(p1)

# 2) Residuals vs Decline — by unit & wood (smooth)
p2 <- ggplot(dfp, aes(x = decline_raw, y = resid, color = wood)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(se = FALSE) +
  facet_wrap(unit ~ wood, scales = "free_x") +
  labs(
    title = "Residuals vs Decline Index",
    x = "Decline index",
    y = "Residual (Observed − Predicted)"
  ) +
  theme_minimal(base_size = 13)
print(p2)

# 3) Residual distribution (density) — by unit & wood
p3 <- ggplot(dfp, aes(x = resid, fill = wood)) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ unit, nrow = 1) +
  labs(
    title = "Residual Distributions by Unit",
    x = "Residual (Observed − Predicted)",
    y = "Density",
    fill = "Class"
  ) +
  theme_minimal(base_size = 13)
print(p3)

# 4) Growth vs Total Volume — observed vs model (smoothed curves)
plot_long <- dfp %>%
  select(unit, wood, total_vol, obs = growth_yr, pred) %>%
  tidyr::pivot_longer(c(obs, pred), names_to = "series", values_to = "value")
p4 <- ggplot(plot_long, aes(x = total_vol, y = value, color = series)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4) +
  facet_grid(unit ~ wood, scales = "free_x") +
  scale_color_manual(values = c(obs = "grey40", pred = "#2C7FB8"),
                     labels = c(obs = "Observed", pred = "Predicted"),
                     name = "") +
  labs(
    title = "Observed vs Predicted Growth Across Stocking",
    x = "Total volume (cords/acre)",
    y = "Annual growth (cords/acre/yr)"
  ) +
  theme_minimal(base_size = 13)
print(p4)

# 5) Calibration curve by decline bins — mean observed vs mean predicted
df_bin <- dfp %>%
  mutate(decline_bin = cut(decline_raw, breaks = seq(0, 1.2, by = 0.1), include.lowest = TRUE)) %>%
  group_by(unit, wood, decline_bin) %>%
  summarise(
    decline_mid = mean(pmin(pmax(decline_raw, 0), 1.2), na.rm = TRUE),
    obs_mean    = mean(growth_yr, na.rm = TRUE),
    pred_mean   = mean(pred,      na.rm = TRUE),
    n           = dplyr::n(),
    .groups = "drop"
  )

p5 <- ggplot() +
  geom_line(data = df_bin, aes(x = decline_mid, y = pred_mean, color = "Predicted"), linewidth = 1) +
  geom_point(data = df_bin, aes(x = decline_mid, y = obs_mean, color = "Observed"), alpha = 0.7) +
  facet_grid(unit ~ wood) +
  scale_color_manual(values = c("Observed" = "grey30", "Predicted" = "#2C7FB8"), name = "") +
  labs(
    title = "Mean Observed vs Predicted by Decline Bin",
    x = "Decline index (binned mid-point)",
    y = "Annual growth (cords/acre/yr)"
  ) +
  theme_minimal(base_size = 13)
print(p5)

# 6) Residuals by total volume — check structure
p6 <- ggplot(dfp, aes(total_vol, resid, color = wood)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ unit, nrow = 1) +
  labs(
    title = "Residuals Across Total Volume",
    x = "Total volume (cords/acre)",
    y = "Residual (Observed − Predicted)"
  ) +
  theme_minimal(base_size = 13)
print(p6)

# QUADRATIC AND GAUSSIAN FUNCTIONS

models_by_wood <- long_df |>
  group_by(wood) |>
  group_map( ~ {
    lm(growth_yr ~ total_vol + I(total_vol^2), data = .x)
  })

# Quadratic model
fit_quadratic <- lm(growth_yr ~ total_vol + I(total_vol^2), data = long_df)

# Gaussian model (nonlinear least squares)
fit_gaussian <- nls(
  growth_yr ~ a * exp(-((total_vol - mu)^2) / (2 * sigma^2)),
  data = long_df,
  start = list(a = 0.6, mu = 25, sigma = 10)
)

# Power-law model (for comparison)
fit_power <- nls(
  growth_yr ~ s - (s - m) * (total_vol / max(total_vol))^p,
  data = long_df,
  start = list(s = 0.6, m = 0.2, p = 1.2)
)

par(mfrow = c(1, 3))  # 3 plots side-by-side

plot(residuals(fit_quadratic) ~ long_df$total_vol, main = "Quadratic Residuals")
abline(h = 0, col = "red")

plot(residuals(fit_gaussian) ~ long_df$total_vol, main = "Gaussian Residuals")
abline(h = 0, col = "red")

plot(residuals(fit_power) ~ long_df$total_vol, main = "Power-Law Residuals")
abline(h = 0, col = "red")


library(tidyverse)

# Define the Gaussian fitting function
fit_gaussian_model <- function(df) {
  tryCatch(
    nls(
      growth_yr ~ a * exp(-((total_vol - mu)^2) / (2 * sigma^2)),
      data = df,
      start = list(a = 0.6, mu = 25, sigma = 10),
      control = nls.control(maxiter = 100)
    ),
    error = function(e) return(NULL)
  )
}

# Nest by unit and wood
model_fits <- long_df %>%
  group_by(unit, wood) %>%
  nest() %>%
  mutate(fit = map(data, fit_gaussian_model),
          params = map(fit, ~ if (!is.null(.x)) coef(.x) else NA)) |>
  unnest_wider(params, names_sep = "_")  # optional: split params into columns

  library(broom)
  library(tidyr)
  library(dplyr)
  library(purrr)

  # Add predictions and residuals
  model_fits <- model_fits %>%
    mutate(
      augmented = map2(fit, data, ~ if (!is.null(.x)) augment(.x, data = .y) else NULL)
    )

  predicted_data <- model_fits %>%
    select(unit, wood, augmented) %>%
    unnest(augmented)

  # Plot predicted vs actual
  ggplot(predicted_data, aes(x = .fitted, y = growth_yr)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    facet_grid(unit ~ wood) +
    labs(title = "Predicted vs Observed Growth",
         x = "Predicted", y = "Observed") +
    theme_minimal()

  # Residuals by volume
  ggplot(predicted_data, aes(x = total_vol, y = .resid)) +
    geom_point(alpha = 0.4) +
    geom_smooth(se = FALSE, method = "loess") +
    facet_grid(unit ~ wood) +
    labs(title = "Residuals vs Total Volume",
         y = "Residuals", x = "Total Volume") +
    theme_minimal()


  model_fits <- model_fits %>%
    mutate(
      rmse = map_dbl(augmented, ~ if (!is.null(.x)) sqrt(mean((.x$.resid)^2)) else NA),
      mae  = map_dbl(augmented, ~ if (!is.null(.x)) mean(abs(.x$.resid)) else NA)
    )
  model_fits
  # >   model_fits - UNTAMPERED
  # # A tibble: 4 × 10
  # # Groups:   unit, wood [4]
  # unit  wood  data               fit    params_a params_mu params_sigma augmented            rmse   mae
  # <chr> <fct> <list>             <list>    <dbl>     <dbl>        <dbl> <list>              <dbl> <dbl>
  #   1 RY    HW    <tibble [315 × 9]> <nls>     0.532      28.3         22.6 <tibble [315 × 11]> 0.166 0.133
  # 2 RY    SW    <tibble [263 × 9]> <nls>     0.620      29.4         20.4 <tibble [263 × 11]> 0.192 0.162
  # 3 SP    HW    <tibble [170 × 9]> <nls>     0.537      32.0         24.5 <tibble [170 × 11]> 0.161 0.128
  # 4 SP    SW    <tibble [268 × 9]> <nls>     0.580      26.5         21.5 <tibble [268 × 11]> 0.206 0.172

  Adjusted_90_Model_Fits <-
    model_fits |> mutate(
      params_a = params_a*.9
    )

  Adjusted_90_Model_Fits

  library(dplyr)
  library(tidyr)
  library(purrr)

  # Define range of total volume and township list
  vol_range <- 0:60
  townships <- c("Davis", "T13R5", "T15R13")

  # Generate all hw/sw combinations per total volume
  volume_grid <- map_dfr(vol_range, function(total_vol) {
    tibble(hw_volume = 0:total_vol, sw_volume = total_vol - hw_volume, total_volume = total_vol)
  })

  # Cross with townships
  input_grid <- crossing(township = townships, volume_grid)

  # Calculate growth using both functions
  output <- input_grid %>%
    rowwise() %>%
    mutate(
      rate_power = calculate_growth_rate(township, hw_volume, sw_volume),
      rate_gauss = calculate_growth_rate2(township, hw_volume, sw_volume)
    ) %>%
    ungroup()

  # Preview result
  print(head(output))

  # Pivot longer for plotting
  growth_long <- output %>%
    pivot_longer(cols = c(rate_power, rate_gauss),
                 names_to = "model", values_to = "growth_rate")

  # Plot
  ggplot(growth_long, aes(x = total_volume, y = growth_rate, color = model)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~township, scales = "free_y") +
    labs(
      title = "Growth Rate Comparison by Model and Township",
      x = "Total Volume (cords/acre)",
      y = "Annual Growth Rate (cords/acre/year)",
      color = "Model"
    ) +
    theme_minimal()

  growth_summary <- output %>%
    group_by(township, total_volume) %>%
    summarise(
      mean_power = mean(rate_power, na.rm = TRUE),
      mean_gauss = mean(rate_gauss, na.rm = TRUE),
      diff = mean_gauss - mean_power,
      pct_diff = 100 * (mean_gauss - mean_power) / mean_power
    ) %>%
    ungroup()

  summary(growth_summary$pct_diff)


  # Filter for the 12–22 cord range
  focused_df <- growth_long %>%
    filter(total_volume >= 35, total_volume <= 55)

  # Summarize average and difference per model and township
  summary_table <- focused_df %>%
    pivot_wider(names_from = model, values_from = growth_rate) %>%
    mutate(
      diff = rate_gauss - rate_power,
      pct_diff = 100 * (rate_gauss - rate_power) / rate_power
    ) %>%
    group_by(township) %>%
    summarise(
      avg_power = mean(rate_power, na.rm = TRUE),
      avg_gauss = mean(rate_gauss, na.rm = TRUE),
      mean_diff = mean(diff, na.rm = TRUE),
      mean_pct_diff = mean(pct_diff, na.rm = TRUE)
    )

  print(summary_table)
