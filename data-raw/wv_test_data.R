# July 15, 2024

# Simulate data for the gross range test
# Data constructed so there are 4 observations per month
# Day 1: Gross Range Flag 4 (low)
# Day 5: Gross Range Flag 3 (low)*
# Day 10: Gross Range Flag 1
# Day 15: Gross Range Flag 4 (high)
# Day 28: Gross Range Flag 3 (high)


library(dplyr)
library(lubridate)
library(here)
library(sensorstrings)
library(qaqcmar)
library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime days
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------
path <- here("inst/testdata")

dat_raw <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt") %>%
  wv_assign_cf_variable_names() %>%
  wv_assign_short_variable_names() #%>%

# for the sensor_depth_to_trim test
dat_raw[1, "sensor_depth_below_surface_m"] <- 10
dat_raw[2, "sensor_depth_below_surface_m"] <- 11.5
dat_raw[nrow(dat_raw) - 1, "sensor_depth_below_surface_m"] <- 13.5
dat_raw[nrow(dat_raw), "sensor_depth_below_surface_m"] <- 15

dat_raw <- dat_raw %>%
  wv_pivot_vars_longer(first_pivot_col = 2)

ts_summary <- dat_raw %>%
  filter(value > 0) %>%
  group_by(variable) %>%
  summarise(
    mean_var = round(mean(value), digits = 3),
    sd_var = round(sd(value), digits = 3),
    median_var = round(median(value), digits = 3)
  ) %>%
  ungroup()

dat <- dat_raw %>%
  filter(
    str_detect(variable, "height|period|to_direction|depth"),
    variable != "sea_water_to_direction_degree"
  ) %>%
  left_join(ts_summary, by = "variable") %>%
  mutate(
    day_utc = day(timestamp_utc),
    # make sure all other values are "good"
    value = if_else(value <= 0, mean_var, value),

    # add "bad" values at known timestamps
    value = case_when(
      day_utc == 1 & variable == "significant_height_m" ~ 0,
      day_utc == 1 & variable == "peak_period_s" ~ -0.1,

      # these ones should NOT be flagged
      day_utc == 2 & variable == "significant_height_m" ~ 0,

      day_utc == 2 & variable == "average_height_largest_33_percent_m" ~ -0.1,
      day_utc == 2 & variable == "period_largest_33_percent_s" ~ -0.1,

      day_utc == 3 & variable == "average_height_largest_10_percent_m" ~ -0.1,
      day_utc == 3 & variable == "period_largest_10_percent_s" ~ -0.1,

      day_utc == 4 & variable == "maximum_height_m" ~ 0,
      day_utc == 4 & variable == "period_maximum_s" ~ -0.1,

      day_utc == 5 & variable == "to_direction_degree" ~ -10,
      day_utc == 6 & variable == "to_direction_degree" ~ 400,
      TRUE ~ value
    )
  ) %>%
  select(-c(mean_var, sd_var, median_var)) %>%
  pivot_wider(names_from = variable, values_from = value)

dat %>%
  wv_pivot_vars_longer(first_pivot_col = 3) %>%
  wv_plot_ts(scales = "free_y", n_col = 2)

# Export rds file
saveRDS(dat, file = here("inst/testdata/wv_test_data.RDS"))

