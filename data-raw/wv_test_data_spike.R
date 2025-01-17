# October 3, 2024

# Simulate data for the Spike test

# Day 2: Spike Flag 4 (low)
# Day 3: Spike Flag 3 (low)
# Day 15: Spike Flag 4 (high)
# Day 27: Spike Flag 3 (high)

library(dplyr)
library(lubridate)
library(here)
library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime day
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------

# Raw data ----------------------------------------------------------------
path <- here("inst/testdata")

stats <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt") %>%
  wv_assign_cf_variable_names() %>%
  wv_assign_short_variable_names() %>%
  wv_test_grossrange(county = "Yarmouth") %>%
  wv_pivot_flags_longer(qc_tests = "grossrange") %>%
  filter(grossrange_flag_value != 4) %>%
  group_by(variable) %>%
  summarise(mean = round(mean(value), digits = 3)) %>%
  ungroup()

spike <- wv_thresholds %>%
  filter(county == "Yarmouth" | is.na(county), qc_test == "spike") %>%
  select(-c(qc_test, county)) %>%
  pivot_wider(values_from = "threshold_value", names_from = "threshold")

vars <- unique(spike$variable)

# simulate data
timestamp_utc = seq(
  as_datetime("2023-01-01 12:00:00"), as_datetime("2023-01-07 12:00:00"),
  by = paste0(60, " mins")
)

dat <- expand.grid(variable = vars, timestamp_utc = timestamp_utc) %>%
  left_join(spike, by = "variable") %>%
  left_join(stats, by = "variable") %>%
  rename(value = mean) %>%
  group_by(variable) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    county = "Yarmouth",
    station = "Cross Island",
    deployment_id = "YR001",
    day_utc = day(timestamp_utc),
    hour_utc = hour(timestamp_utc),

    lag_value = lag(value),
    lead_value = lead(value),
    spike_ref = abs((lag_value + lead_value) / 2),

    value = case_when(
      day_utc == 3 & hour_utc == 0 ~ spike_ref + 1.1 * spike_low,
      day_utc == 3 & hour_utc == 12 ~ spike_ref - 1.1 * spike_low,

      day_utc == 5 & hour_utc == 0 ~ spike_ref + 1.1 * spike_high,
      day_utc == 5 & hour_utc == 12 ~ spike_ref - 1.1 * spike_high,
      TRUE ~ value
    )
  ) %>%
  ungroup() %>%
  select(
    county, station, deployment_id, timestamp_utc, day_utc, hour_utc,
    variable, value
    #spike_low, spike_high, lag_value, lead_value, spike_ref
  ) %>%
 # select(-c(spike_low, spike_high, lag_value, lead_value, spike_ref)) %>%
  pivot_wider(values_from = "value", names_from = "variable")

wv_plot_ts(dat, n_col = 2, scales = "free_y")

dat %>%
  wv_test_spike(county = "Yarmouth") %>%
  wv_pivot_flags_longer(qc_test = "spike") %>%
  wv_plot_flags(qc_tests = "spike")

# Export rds file
saveRDS(dat, file = here("inst/testdata/wv_test_data_spike.RDS"))



