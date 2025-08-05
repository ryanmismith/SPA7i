library(tidyverse)
library(dplyr)
library(SPA7i)

setwd("~/R/SPA7i")

# calibrate_decline_model.R
#
# Starting with your exported CSV: rangeley_growth_long.csv
# This file must have: year, stand, volume, hw_volume, sw_volume, growth_obs_yr

# Rangeley Hardwoods ------------------------------------------------------


#── 1. Read your long‐form growth data ─────────────────────────────────────────
calib_df <- read_csv(
  "rangeley_growth_long.csv",
  col_types = cols(
    Year          = col_double(),   # census year
    stand         = col_character(),# stand/matrix identifier
    volume        = col_double(),   # total cords/acre
    growth_5yr = col_double()
  )
) |> mutate(
  growth_obs_yr = growth_5yr/5, # observed annual growth (cords/acre/yr)
  hw_volume = case_when(
    stand_type == "H" ~ volume*.85,
    stand_type == "HS" ~ volume*.65
  ),
  sw_volume = volume - hw_volume
  ) |> rename(
    year = 'Year'
  )

#── 2. Compute auxiliary columns ────────────────────────────────────────────────
calib_df <- calib_df %>%
  rowwise() %>%                          # ⬅️ switch into “one row at a time” mode
  mutate(
    current_volume = hw_volume + sw_volume,
    max_volume     = calculate_max_volume(
      hw_volume,
      sw_volume,
      maxvol = 40      # your new biological max
    ),
    decline_raw    = pmax(0, (current_volume - 14) / (max_volume - 14))
  ) %>%
  ungroup() %>%                          # ⬅️ back out of rowwise
  filter(!is.na(growth_obs_yr))

calib_df <- calib_df %>%
  mutate(growth_target = growth_obs_yr - 0.03)

# A1. Subset to the “no-decline” zone
start_df <- calib_df %>%
  filter(decline_raw < 0.05)

# 2. Fit growth ≈ SW_rate + (HW_rate − SW_rate)*frac_hw
lm_start <- lm(growth_target ~ frac_hw, data = start_df)

# 3. Extract rates
sw_max_rate_est <- coef(lm_start)[1]
hw_max_rate_est <- coef(lm_start)[1] + coef(lm_start)[2]

cat("Estimated softwood max rate:", round(sw_max_rate_est,3), "\n")
cat("Estimated hardwood max rate:", round(hw_max_rate_est,3), "\n")

# 4. (Optional) check significance
summary(lm_start)

# 2. Fix your “maximum” growth‐rate coefficients:
sw_max_rate <- 0.55    # assumed max annual growth for 100% softwood
hw_max_rate <- 0.508   # assumed max annual growth for 100% hardwood
base_vol     <- 14     # below this, no decline applies


# 3. Compute start_rate as a weighted blend of those maxima for each row:
calib_df <- calib_df %>%
  mutate(
    # fraction of volume that is SW vs HW
    frac_sw = sw_volume / (hw_volume + sw_volume),
    frac_hw = hw_volume / (hw_volume + sw_volume),

    # start_rate: the growth rate when Volume = 14 (i.e. no decline yet)
    start_rate = sw_max_rate * frac_sw +
      hw_max_rate * frac_hw
  )

#──  Power-law decline model fit ────────────────────────────────────────────────

# Model we fit:
#
#        growth_obs_yr ≈ start_rate
#                           − (start_rate − min_rate) × decline_raw^p
#
#  • p        controls *how sharply* growth falls off as volume increases.
#             – p = 2 → exactly quadratic decline (your original code).
#             – p < 2 → slower decline; p > 2 → steeper decline.
#  • min_rate is the *floor* growth rate when current_volume → max_volume.
#
# We use R’s `nls()` to find (p, min_rate) that **minimize the sum of squared residuals**:
#    Σ_i [growth_obs_yr(i) − model(i)]^2
#
# Under the hood, `nls()` runs a variant of the Gauss–Newton algorithm (here the
# “port” variant) with our supplied parameter bounds to ensure:
#    0.1 ≤ p ≤ 10    and    0 ≤ min_rate ≤ 1.

power_fit <- nls(
  formula   = growth_obs_yr ~
    # fixed start_rate from above
    start_rate
  - (start_rate - min_rate) * decline_raw^p,
  data      = calib_df,
  start     = list(
    p        = 2.0,   # initial guess = quadratic
    min_rate = 0.02   # initial guess = your original floor
  ),
  algorithm = "port",      # supports parameter bounds
  lower     = c(p = 0.1,   # prevent exponent collapse
                min_rate = 0),
  upper     = c(p = 10,    # cap exponent for numeric stability
                min_rate = 1)
)

#──  Inspect the fitted parameters ──────────────────────────────────────────────
power_params <- coef(power_fit)
cat("Fitted power-law exponent p =", power_params["p"], "\n")
cat("Fitted minimum growth rate min_rate =", power_params["min_rate"], "\n")

# view a full summary:
summary(power_fit)

#── 4. Exponential decline model ──────────────────────────────────────────────
# Model form:
#   growth_obs ≈ min_rate +
#     (start_rate - min_rate) * exp(-k * current_volume / max_volume)
#
# where:
# - k        = decay constant to estimate
# - min_rate = floor growth rate
expo_fit <- nls(
  formula = growth_obs_yr ~
    min_rate +
    (
      (sw_max_rate * (sw_volume/current_volume) +
         hw_max_rate * (hw_volume/current_volume))
      - min_rate
    ) * exp(-k * current_volume / max_volume),
  data      = calib_df,
  start     = list(k = 1,      # initial guess
                   min_rate = 0.02),
  algorithm = "port",
  lower     = c(k = 0.01,      # bound k away from zero
                min_rate = 0),
  upper     = c(k = 10,        # prevent extreme decay
                min_rate = 1)
)

summary(expo_fit)
#── 5. Extract and report fitted parameters ────────────────────────────────────
power_params <- coef(power_fit)
expo_params  <- coef(expo_fit)

cat("Power model (p, min_rate):\n")
print(power_params)
#   p       min_rate

cat("Exponential model (k, min_rate):\n")
print(expo_params)
#   k       min_rate

#── 6. Attach predictions & residuals ──────────────────────────────────────────
calib_df <- calib_df %>%
  mutate(
    # recompute start_rate here for clarity
    start_rate = sw_max_rate * (sw_volume/current_volume) +
      hw_max_rate * (hw_volume/current_volume),

    # predict with power model
    pred_power = predict(power_fit, newdata = .),
    resid_power = growth_obs_yr - pred_power,

    # predict with expo model
    pred_expo  = predict(expo_fit, newdata = .),
    resid_expo  = growth_obs_yr - pred_expo
  )

#── 7. Visualizations ─────────────────────────────────────────────────────────
# a) Observed vs volume with both fits
p1 <- ggplot(calib_df, aes(current_volume, growth_obs_yr)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_power), color = "blue", linewidth = 1) +
  geom_line(aes(y = pred_expo),  color = "red",  linewidth = 1) +
  labs(
    title    = "Observed annual growth vs total volume",
    subtitle = "Blue = power‐law; Red = exponential",
    x        = "Total volume (cords/acre)",
    y        = "Annual growth (cords/acre/yr)"
  )

# b) Residuals vs volume
p2 <- ggplot(calib_df) +
  geom_point(aes(current_volume, resid_power), color = "blue", alpha = 0.3) +
  geom_point(aes(current_volume, resid_expo),  color = "red",  alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title    = "Model residuals by volume",
    subtitle = "Blue = power‐law; Red = exponential",
    x        = "Total volume (cords/acre)",
    y        = "Residual (obs − pred)"
  )

# c) Histogram of residuals
p3 <- calib_df %>%
  select(resid_power, resid_expo) %>%
  pivot_longer(everything(), names_to="model", values_to="resid") %>%
  ggplot(aes(resid, fill = model)) +
  geom_histogram(position="identity", alpha=0.5, bins=30) +
  labs(
    title    = "Residual distributions",
    subtitle = "Power vs Exponential decline",
    x        = "Residual (cords/acre/yr)",
    y        = "Count"
  )

# Print all three
print(p1)
print(p2)
print(p3)

# Interpretation
#
# Power model exponent p > 2 ⇒ sharper‐than‐quadratic decline; p < 2 ⇒ slower drop‐off.
# k in the exponential model controls how fast growth decays per unit volume fraction.
# Inspect p2 (residuals vs volume) for any remaining bias; you want points scattered evenly around zero.
# Inspect p3 (histograms) for which model yields narrower, more centered residuals.

# Use the set of parameters from your best‐performing model in your production
# calculate_growth_rate()—replacing the hard‐coded ^2 or exp(…) multiplier.


# SP Softwoods ------------------------------------------------------------


# calibrate_softwood_decline.R
#
# 1) Read your wide softwood table
#    — first column “Row Labels” = year, then one column per stand:
#      S1A-N, S1A-P, …, S4D-P, SH1A-N, SH1B-N, …
library(tidyverse)

softwide <- read_csv("SPSWGrowOnlyVols.csv",
                     col_types = cols(.default = col_double(),
                                      `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`)

# 2) Pivot to long form
softlong <- softwide %>%
  pivot_longer(-year, names_to="stand", values_to="volume") %>%
  filter(!is.na(volume))  # drop any empty cells

# 3) Assign softwood fraction by stand type:
#    • “S” stands (e.g. S1A-N)   = 85% SW, 15% HW
#    • “SH” stands (e.g. SH1A-N) = 65% SW, 35% HW
softlong <- softlong %>%
  mutate(
    frac_sw = case_when(
      str_starts(stand, "SH") ~ 0.65,
      str_starts(stand, "S")  ~ 0.85,
      TRUE                    ~ NA_real_
    ),
    frac_hw = 1 - frac_sw,
    hw_volume = volume * frac_hw,
    sw_volume = volume * frac_sw
  )

# 4) Compute the same decline variables
sw_max_rate <- 0.60    # your chosen biology
hw_max_rate <- 0.538
base_vol     <- 14

# Before your pipeline, wrap the scalar function:
calc_max_vol <- Vectorize(
  calculate_max_volume,
  vectorize.args = c("hw_volume", "sw_volume")
)

# Then in your mutate, use the wrapper:
calib_df <- softlong %>%
  arrange(stand, year) %>%
  mutate(
    total_vol   = hw_volume + sw_volume,
    start_rate  = sw_max_rate * frac_sw + hw_max_rate * frac_hw,
    # use the vectorized version here:
    max_volume  = calc_max_vol(hw_volume, sw_volume, maxvol = 38),
    decline_raw = pmax(0, (total_vol - base_vol) / (max_volume - base_vol)),

    growth_5yr    = volume - lag(volume),
    growth_obs_yr = growth_5yr / (year - lag(year))
  ) %>%
  filter(!is.na(growth_obs_yr))

# 5) Power‐law fit (only p & min_rate estimated)
power_fit <- nls(
  growth_obs_yr ~ start_rate - (start_rate - min_rate)*decline_raw^p,
  data      = calib_df,
  start     = list(p = 2.0, min_rate = 0.02),
  algorithm = "port",
  lower     = c(p = 0.1, min_rate = 0),
  upper     = c(p = 10,  min_rate = 1)
)
power_params <- coef(power_fit)
summary(power_fit)
# 6) Exponential fit on normalized decline_raw
expo_fit <- nls(
  growth_obs_yr ~ min_rate + (start_rate - min_rate)*exp(-k*decline_raw),
  data      = calib_df,
  start     = list(k = 1.0, min_rate = 0.02),
  algorithm = "port",
  lower     = c(k = 0.001, min_rate = 0),
  upper     = c(k = 20,    min_rate = 1)
)
expo_params <- coef(expo_fit)

# 7) Quick diagnostics
cat("Softwood power-law (p, min_rate):\n"); print(power_params)
cat("Softwood expo (k, min_rate):\n");    print(expo_params)

# 8) (Optional) Visualize just like before
calib_df <- calib_df %>%
  mutate(
    pred_power  = predict(power_fit, newdata = .),
    pred_expo   = predict(expo_fit,  newdata = .),
    resid_power = growth_obs_yr - pred_power,
    resid_expo  = growth_obs_yr - pred_expo
  )

library(ggplot2)
ggplot(calib_df, aes(total_vol, growth_obs_yr)) +
  geom_point(alpha=0.3) +
  geom_line(aes(y=pred_power), color="blue") +
  geom_line(aes(y=pred_expo),  color="red") +
  labs(title="Softwood stands: Power vs Expo decline",
       x="Total volume (cords/acre)", y="Annual growth")



# SP HWs ------------------------------------------------------------------

hardwood <- read_csv("SPHWGrowOnlyVols.csv",
                     col_types = cols(.default = col_double(),
                                      `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`)

# 2) Pivot to long form
hardlong <- hardwood %>%
  pivot_longer(-year, names_to="stand", values_to="volume") %>%
  filter(!is.na(volume))  # drop any empty cells

# 3) Assign softwood fraction by stand type:
#    • “S” stands (e.g. S1A-N)   = 85% SW, 15% HW
#    • “SH” stands (e.g. SH1A-N) = 65% SW, 35% HW
hardlong <- hardlong %>%
  mutate(
    frac_hw = case_when(
      str_starts(stand, "HS") ~ 0.65,
      str_starts(stand, "H")  ~ 0.85,
      TRUE                    ~ NA_real_
    ),
    frac_sw = 1 - frac_hw,
    hw_volume = volume * frac_hw,
    sw_volume = volume * frac_sw
  )

# 4) Compute the same decline variables
sw_max_rate <- 0.60    # your chosen biology
hw_max_rate <- 0.538
base_vol     <- 14

# Before your pipeline, wrap the scalar function:
calc_max_vol <- Vectorize(
  calculate_max_volume,
  vectorize.args = c("hw_volume", "sw_volume")
)

# Then in your mutate, use the wrapper:
calib_df <- hardlong %>%
  arrange(stand, year) %>%
  group_by(stand) %>%                             # ← make lag() per‐stand
  mutate(
    total_vol   = hw_volume + sw_volume,
    start_rate  = sw_max_rate * frac_sw + hw_max_rate * frac_hw,

    # vectorized max‐volume as before
    max_volume  = calc_max_vol(hw_volume, sw_volume, maxvol = 38),
    decline_raw = pmax(0, (total_vol - base_vol) / (max_volume - base_vol)),

    # now lag only looks within each stand, and we divide by 5
    growth_5yr    = volume - lag(volume),
    growth_obs_yr = growth_5yr / 5,
    growth_year = growth_obs_yr - .03
  ) %>%
  ungroup() %>%                                   # back to regular data‐frame
  filter(!is.na(growth_obs_yr)) |>
  filter(growth_year > 0)

# 5) Power‐law fit (only p & min_rate estimated)
power_fit <- nls(
  growth_year ~ start_rate - (start_rate - min_rate)*decline_raw^p,
  data      = calib_df,
  start     = list(p = 1.5, min_rate = 0.4),
  algorithm = "port",
  lower     = c(p = 0.01,  min_rate = 0.0),
  upper     = c(p = 20,    min_rate = 1.0),
  control   = nls.control(maxiter = 100, tol = 1e-4, minFactor = 1/1024)
)

power_params <- coef(power_fit)
summary(power_fit)

# 6) Exponential fit on normalized decline_raw
expo_fit <- nls(
  growth_obs_yr ~ min_rate + (start_rate - min_rate)*exp(-k*decline_raw),
  data      = calib_df,
  start     = list(k = 1.0, min_rate = 0.02),
  algorithm = "port",
  lower     = c(k = 0.001, min_rate = 0),
  upper     = c(k = 20,    min_rate = 1)
)
expo_params <- coef(expo_fit)

# 7) Quick diagnostics
cat("Hardwood power-law (p, min_rate):\n"); print(power_params)
cat("Hardwood expo (k, min_rate):\n");    print(expo_params)

# 8) (Optional) Visualize just like before
calib_df <- calib_df %>%
  mutate(
    pred_power  = predict(power_fit, newdata = .),
    pred_expo   = predict(expo_fit,  newdata = .),
    resid_power = growth_obs_yr - pred_power,
    resid_expo  = growth_obs_yr - pred_expo
  )

library(ggplot2)
ggplot(calib_df, aes(total_vol, growth_obs_yr)) +
  geom_point(alpha=0.3) +
  geom_line(aes(y=pred_power), color="blue") +
  geom_line(aes(y=pred_expo),  color="red") +
  labs(title="Softwood stands: Power vs Expo decline",
       x="Total volume (cords/acre)", y="Annual growth")


# calibrate_mixed_decline.R
#
# This assumes you have:
#  • calculate_max_volume(hw, sw, maxvol=38) already defined
#  • tidyverse loaded

library(tidyverse)

# 1) Read in your wide table
mixed_wide <- read_csv("RY_GrowOnly.csv",
                       col_types = cols(.default = col_double(),
                                        `Row Labels` = col_integer())) %>%
  rename(year = `Row Labels`)

# 2) Pivot to long: one row per (stand, year, volume)
mixed_long <- mixed_wide %>%
  pivot_longer(-year, names_to="stand", values_to="volume") %>%
  filter(!is.na(volume))

# 3) Assign softwood fraction by the new prefixes:
#     OS = 80% SW, SH = 70% SW,  S = 90% SW,  HS = 30% SW,  H = 10% SW
mixed_long <- mixed_long %>%
  mutate(
    frac_sw = case_when(
      str_starts(stand, "OS")  ~ 0.80,
      str_starts(stand, "SH")  ~ 0.70,
      str_starts(stand, "HS")  ~ 0.30,
      str_starts(stand, "S")   ~ 0.90,
      str_starts(stand, "H")   ~ 0.10,
      TRUE                     ~ NA_real_
    ),
    frac_hw   = 1 - frac_sw,
    # split your measured total into its HW/SW components
    hw_volume = volume * frac_hw,
    sw_volume = volume * frac_sw
  )

# 4) Constants from your biological knowledge
sw_max_rate <- 0.60    # max annual growth if 100% SW
hw_max_rate <- 0.538   # max annual growth if 100% HW
base_vol     <- 14     # no decline below 14 cords/acre

# 5) Vectorized wrapper so calculate_max_volume() handles a full column
calc_max_vol <- Vectorize(
  calculate_max_volume,
  vectorize.args = c("hw_volume", "sw_volume")
)

# 6) Compute per‐row decline inputs & observed annual growth
calib_df <- mixed_long %>%
  arrange(stand, year) %>%
  group_by(stand) %>%     # ensures lag() only looks within each stand
  mutate(
    total_vol    = hw_volume + sw_volume,

    # (A) fixed “start rate” from your max‐rate assumptions
    start_rate   = sw_max_rate * frac_sw + hw_max_rate * frac_hw,

    # (B) biological maximum at each point
    max_volume   = calc_max_vol(hw_volume, sw_volume, maxvol = 38),

    # (C) normalized “decline fraction” 0→1 as volume goes base_vol→max
    decline_raw  = pmax(0, (total_vol - base_vol) / (max_volume - base_vol)),

    # (D) true 5-yr change, converted to per-year
    growth_5yr    = volume - lag(volume),
    growth_obs_yr = growth_5yr / 5
  ) %>%
  ungroup() %>%
  filter(!is.na(growth_obs_yr))

# 7) Fit power-law decline (only p & min_rate estimated)
power_fit <- nls(
  growth_obs_yr ~ start_rate - (start_rate - min_rate) * decline_raw^p,
  data      = calib_df,
  start     = list(p = 2.0,    min_rate = 0.02),
  algorithm = "port",
  lower     = c(p = 0.1,       min_rate = 0),
  upper     = c(p = 10,        min_rate = 1)
)
power_params <- coef(power_fit)

# 8) Fit exponential decline on normalized decline_raw (k & min_rate)
expo_fit <- nls(
  growth_obs_yr ~ min_rate + (start_rate - min_rate) * exp(-k * decline_raw),
  data      = calib_df,
  start     = list(k = 1.0,    min_rate = 0.02),
  algorithm = "port",
  lower     = c(k = 0.001,     min_rate = 0),
  upper     = c(k = 20,        min_rate = 1)
)
expo_params <- coef(expo_fit)

# 9) Report results
cat("Power-law decline parameters (p, min_rate):\n")
print(power_params)

cat("\nExponential decline parameters (k, min_rate):\n")
print(expo_params)

# 10) (Optional) Visual diagnostic
calib_df <- calib_df %>%
  mutate(
    pred_power  = predict(power_fit, newdata = .),
    pred_expo   = predict(expo_fit,  newdata = .),
    resid_power = growth_obs_yr - pred_power,
    resid_expo  = growth_obs_yr - pred_expo
  )

ggplot(calib_df, aes(total_vol, growth_obs_yr)) +
  geom_point(alpha=0.3) +
  geom_line(aes(y=pred_power), color="blue", size=1) +
  geom_line(aes(y=pred_expo),  color="red",  size=1) +
  labs(
    title = "Mixed stands: Power vs Exponential decline",
    x     = "Total volume (cords/acre)",
    y     = "Annual growth (cords/acre/yr)"
  )


library(tidyverse)

# 1) Read & pivot
mixed_wide <- read_csv("RY_GrowOnly.csv",
                       col_types = cols(.default=col_double(),
                                        `Row Labels`=col_integer())) %>%
  rename(year=`Row Labels`)

mixed_long <- mixed_wide %>%
  pivot_longer(-year, names_to="stand", values_to="volume") %>%
  filter(!is.na(volume))

# 2) Assign fractions & volumes
mixed_long <- mixed_long %>%
  mutate(
    frac_sw = case_when(
      str_starts(stand, "OS") ~ 0.80,
      str_starts(stand, "SH") ~ 0.70,
      str_starts(stand, "HS") ~ 0.30,
      str_starts(stand, "S")  ~ 0.90,
      str_starts(stand, "H")  ~ 0.10,
      TRUE                    ~ NA_real_
    ),
    frac_hw   = 1 - frac_sw,
    sw_volume = volume * frac_sw,
    hw_volume = volume * frac_hw
  )

# 3) Compute observed annual growth
base_vol  <- 14
maxvol    <- 38

calc_max_vol <- Vectorize(calculate_max_volume,
                          vectorize.args=c("hw_volume","sw_volume"))

calib_df <- mixed_long %>%
  arrange(stand, year) %>%
  group_by(stand) %>%
  mutate(
    total_vol    = sw_volume + hw_volume,
    max_volume   = calc_max_vol(hw_volume, sw_volume, maxvol = maxvol),
    decline_raw  = pmax(0, (total_vol - base_vol)/(max_volume - base_vol)),
    growth_obs_yr = (volume - lag(volume)) / 5
  ) %>%
  ungroup() %>%
  filter(!is.na(growth_obs_yr))

# 4) Define the three parameter sets
params <- tribble(
  ~case, ~sw_start, ~hw_start, ~p,      ~min_rate,
  "SP",   0.55,       0.538,      1.565,   0.30,
  "RY",   0.66,       0.538,      0.661,   0.30,
  "All",  (0.55+0.66)/2, 0.538,  (1.565+0.661)/2,  0.30
)

# 5) Generate predictions for each case
pred_df <- calib_df %>%
  crossing(params) %>%   # expand for each case
  mutate(
    start_rate = sw_start * frac_sw + hw_start * frac_hw,
    pred       = start_rate - (start_rate - min_rate) * decline_raw^p,
    pred       = pmax(pred, min_rate),   # enforce floor
    resid      = growth_obs_yr - pred
  )

# 6) Compute MAE & RMSE by case
metrics <- pred_df %>%
  group_by(case) %>%
  summarise(
    MAE  = mean(abs(resid)),
    RMSE = sqrt(mean(resid^2)),
    .groups="drop"
  )

print(metrics)

# 7) Scatterplot observed vs predicted
pred_df %>%
  ggplot(aes(x=growth_obs_yr, y=pred, color=case)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray50") +
  geom_point(alpha=0.4) +
  coord_equal() +
  facet_wrap(~case) +
  labs(
    title="Observed vs Predicted Annual Growth",
    x="Observed (cords/acre/yr)",
    y="Predicted (cords/acre/yr)"
  ) +
  theme_minimal()

library(tidyverse)

# -- assume you already have `calib_df` with these columns:
#    stand, year, total_vol, frac_sw, frac_hw, decline_raw, growth_obs_yr

# 1) Define the three parameter sets
params <- tribble(
  ~case, ~sw_start, ~hw_start, ~p,     ~min_rate,
  "SP",   0.55,      0.538,     1.565,   0.30,
  "RY",   0.66,      0.538,     0.661,   0.30,
  "All",  (0.55+0.66)/2, 0.538, (1.565+0.661)/2, 0.30
)

base_vol <- 14
maxvol   <- 38

# 2) Expand calib_df by case, compute predictions
pred_df <- calib_df %>%
  crossing(params) %>%               # one row per obs × case
  mutate(
    # start_rate differs by case
    start_rate = sw_start * frac_sw + hw_start * frac_hw,
    # apply the power‐law
    pred       = start_rate
    - (start_rate - min_rate) * decline_raw^p,
    pred       = pmax(pred, min_rate)  # enforce floor
  )

# 3) Plot
ggplot() +
  # raw observed points
  geom_point(
    data = calib_df,
    aes(x = total_vol, y = growth_obs_yr),
    color = "gray60", alpha = 0.3
  ) +
  # predicted curves (connect predictions sorted by volume)
  geom_line(
    data = pred_df %>% arrange(case, total_vol),
    aes(x = total_vol, y = pred, color = case),
    size = 1
  ) +
  scale_color_manual(
    values = c(SP = "#00A08A", RY = "#F2AD00", All = "#E15759")
  ) +
  labs(
    title = "Observed vs. Power‐Law Predictions (SP, RY, All)",
    x     = "Total volume (cords/acre)",
    y     = "Annual growth (cords/acre/yr)",
    color = "Case"
  ) +
  theme_minimal(base_size = 14)

library(tidyverse)

# -- assume `calib_df` and `pred_df` are already in your environment as per above

# 1) Restrict to volumes < 50
obs  <- calib_df  %>% filter(total_vol < 50)
pred <- pred_df    %>% filter(total_vol < 50)

# 2) Plot smoothed prediction curves + raw points
ggplot() +
  geom_smooth(
    data    = obs,
    aes(x = total_vol, y = growth_obs_yr),
    method    = "loess",
    se        = FALSE,
    span      = 0.3,
    size      = 1
  ) +
  # smoothed power‐law prediction curves
  geom_smooth(
    data      = pred,
    aes(x = total_vol, y = pred, color = case),
    method    = "loess",
    se        = FALSE,
    span      = 0.3,
    size      = 1
  ) +
  coord_cartesian(xlim = c(0, 50)) +
  scale_color_manual(
    values = c(SP = "#00A08A", RY = "#F2AD00", All = "#E15759")
  ) +
  labs(
    title = "Observed vs. Smoothed Power-Law Pred"
  )

# — assume you already have:
#    calib_df with columns: total_vol, growth_5yr
#    pred_df  with columns: total_vol, case, pred  (pred is annual rate)

# 1) Build a unified “long” data.frame
obs_df <- calib_df %>%
  filter(total_vol < 50) %>%
  transmute(
    total_vol,
    case  = "Observed",
    value = growth_5yr         # 5-yr observed growth
  )

pred5_df <- pred_df %>%
  filter(total_vol < 50) %>%
  transmute(
    total_vol,
    case  = case,
    value = pred * 5           # convert annual→5-yr growth
  )

plot_df <- bind_rows(obs_df, pred5_df)

# 2) Plot mean curves for each “case”
ggplot(plot_df, aes(x = total_vol, y = value, color = case)) +
  stat_summary(
    fun    = mean,
    geom   = "line",
    size   = 1
  ) +
  coord_cartesian(xlim = c(0, 50)) +
  scale_color_manual(
    values = c(
      Observed = "gray60",
      SP       = "#00A08A",
      RY       = "#F2AD00",
      All      = "#E15759"
    )
  ) +
  labs(
    title = "5-Year Growth: Observed vs. Power-Law Predictions",
    x     = "Total volume (cords/acre)",
    y     = "5-yr growth (cords/acre per 5 yr)",
    color = ""
  ) +
  theme_minimal(base_size = 14)
