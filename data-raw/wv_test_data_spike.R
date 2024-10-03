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
  wv_pivot_vars_longer() %>%
  group_by(variable) %>%
  summarise(mean = round(mean(value), digits = 3))

spike <- wv_thresholds %>%
  filter(county == "Halifax", qc_test == "spike") %>%
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
  mutate(
    county = "Halifax",
    station = "McNabs Island",
    deployment_id = "HL001",
    value = jitter(value),
    day_utc = day(timestamp_utc),
    hour_utc = hour(timestamp_utc),
    value = case_when(
      day_utc == 3 & hour_utc == 0 ~ spike_low,
      day_utc == 3 & hour_utc == 12 ~ -spike_low,

      day_utc == 5 & hour_utc == 0 ~ spike_high,
      day_utc == 5 & hour_utc == 12 ~ -spike_high,

      TRUE ~ value
    )

  )

attr(dat, "out.attrs") <- NULL

wv_plot_ts(dat, n_col = 2, scales = "free_y")

dat <- dat %>%
  select(-c(spike_low, spike_high)) %>%
  pivot_wider(values_from = "value", names_from = "variable") %>%
  wv_test_spike(county = "Halifax")

dat %>%
  wv_pivot_flags_longer(qc_test = "spike") %>%
  wv_plot_flags(qc_tests = "spike")


dat_test <- list()

for (i in seq_along(vars)) {

  var_i <- vars[i]
  sd_i <- roll_sd[which(roll_sd$variable == var_i), ]$threshold_value
  mean_i <- stats[which(stats$variable == var_i),]$mean

  dat_i <- dat %>% filter(variable == var_i)

  set.seed(3256)
  vals_1 <- rnorm(nrow(dat_i), mean = mean_i, sd = sd_i / 4)
  vals_3 <- rnorm(nrow(filter(dat_i, day_utc == 3)), mean = mean_i, sd = 2 * sd_i)

  dat_i <- dat_i %>%
    mutate(value = vals_1)

  dat_i[which(dat_i$day_utc == 3), "value"] <- vals_3

  dat_test[[i]] <- dat_i
}

dat_sd <- map_df(dat_test, .f = bind_rows) %>%
  select(-threshold_value) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(
    county = "Halifax",
    station = "McNabs Island",
    deployment_id = "HL001"
  ) %>%
  select(county, station, deployment_id, everything())









