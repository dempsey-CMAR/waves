# July 15, 2024

# Simulate data for the gross range test
# Data constructed so there are 4 observations per month
# Day 2: Gross Range Flag 4 (low)
# Day 4: Gross Range Flag 3 (low)*
# Day 6: Gross Range Flag 4 (high)
# Day 8: Gross Range Flag 3 (high)


library(dplyr)
library(lubridate)
library(here)
library(sensorstrings)
library(stringr)
library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime days
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------
path <- here("inst/testdata")

dat_raw <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt") %>%
  wv_assign_cf_variable_names() %>%
  wv_assign_short_variable_names() %>%
  mutate(
    timestamp_utc = seq(
      as_datetime("2023-01-01 12:00:00"), as_datetime("2023-01-09 22:00:00"),
      by = paste0(120, " mins")
    ))

gr_thresholds <- wv_thresholds %>%
  filter(qc_test == "grossrange", county == "Halifax" | is.na(county)) %>%
  select(-c(qc_test, county)) %>%
  pivot_wider(values_from = "threshold_value", names_from = "threshold") %>%
  group_by(variable) %>%
  mutate(
    suspect_high = mean(c(gr_max, user_max)),
    pass = mean(c(user_min, user_max))
  ) %>%
  ungroup()

# for the sensor_depth_to_trim test
dat_raw[1, "sensor_depth_below_surface_m"] <- 14.1
dat_raw[2, "sensor_depth_below_surface_m"] <- 13
dat_raw[nrow(dat_raw) - 1, "sensor_depth_below_surface_m"] <- 13.5
dat_raw[nrow(dat_raw), "sensor_depth_below_surface_m"] <- 15

dat_raw <- dat_raw %>%
  wv_pivot_vars_longer()

#dat_raw %>%
#  wv_plot_ts(scales = "free_y", n_col = 2)

dat <- dat_raw %>%
  left_join(gr_thresholds, by = "variable") %>%
  mutate(
    day_utc = day(timestamp_utc),
    # make sure all other values are "good"
    value = if_else(value <= 0, pass, value),
    # Flag 4 (low)
    value = case_when(
      # these ones should NOT be flagged
      day_utc == 1 & variable == "sea_water_speed_m_s" ~ 0.2,
      day_utc == 1 & variable == "significant_height_m" ~ 0,

      day_utc == 2 & variable == "significant_height_m" ~ 0,
      day_utc == 2 & variable == "peak_period_s" ~ -0.1,

      # these SHOULD be flagged
      day_utc == 2 & variable == "average_height_largest_33_percent_m" ~ 0,
      day_utc == 2 & variable == "period_largest_33_percent_s" ~ -0.1,

      day_utc == 2 & variable == "average_height_largest_10_percent_m" ~ 0,
      day_utc == 2 & variable == "period_largest_10_percent_s" ~ -0.1,

      day_utc == 2 & variable == "maximum_height_m" ~ 0,
      day_utc == 2 & variable == "period_maximum_s" ~ -0.1,

      day_utc == 2 & variable %in% c(
        "from_direction_degree",
        "sea_water_speed_m_s",
        "sea_water_to_direction_degree",
        "sensor_depth_below_surface_m"
      ) ~ gr_min - 2,
      # Flag 3 (low)
      # right now all gr_min thresholds = user_min thresholds, so these should
      # actually be flagged as 4
      day_utc == 4 ~ user_min - 1,
      # Flag 4 high
      day_utc == 6 ~ gr_max + 3,
      # Flag 3 high
      day_utc == 8 ~ suspect_high,
      TRUE ~ value
    )
  ) %>%
  select(-c(contains("gr_"), contains("user_"), suspect_high, pass)) %>%
  pivot_wider(names_from = variable, values_from = value)

p <- dat %>%
  wv_pivot_vars_longer() %>%
  wv_plot_ts(scales = "free_y", n_col = 2)

# Export rds file
saveRDS(dat, file = here("inst/testdata/wv_test_data_grossrange.RDS"))

