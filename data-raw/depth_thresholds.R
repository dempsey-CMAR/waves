# July 5, 2024

# evaluate current sensor depth data to determine typical rate of change of
# sensor_depth_at_low_tide_m
# use results to inform flagging sensor_depth_at_low_tide_m data in the wave
# datasets

library(adcp)
library(dplyr)
library(ggplot2)


# import data -------------------------------------------------------------

dat_raw <- adcp_import_data()

dat <- dat_raw %>%
  select(
    county, deployment_id, station, timestamp_utc, sensor_depth_below_surface_m
  ) %>%
  distinct()

# current data is recorded every 10 mins; wave data is recorded every hour
one_hour <- seq(1, nrow(dat), 6)


# calculate depth change --------------------------------------------------

dat_depth <- dat %>%
  slice(one_hour) %>%
  group_by(county, deployment_id, station) %>%
  mutate(
    depth_diff = lead(sensor_depth_below_surface_m) - sensor_depth_below_surface_m,
    time_diff = difftime(lead(timestamp_utc), timestamp_utc)
  ) %>%
  ungroup() %>%
  na.omit()

ggplot(dat_depth, aes(depth_diff)) +
  geom_histogram()

ggplot(dat_depth, aes(depth_diff)) +
  geom_histogram() +
  facet_wrap(~county, ncol = 3)


# check time intervals ----------------------------------------------------

# most, but not all 1 hour time intervals
time_check <-  dat_depth %>%
  group_by(time_diff) %>%
  count() %>%
  arrange(desc(n))

ggplot(dat_depth, aes(time_diff)) +
  geom_histogram()

ggplot(dat_depth, aes(time_diff)) +
    geom_histogram(binwidth = 3600) +
  facet_wrap(~county, ncol = 3, scales = "free_x")


# summary statistics ------------------------------------------------------

dat_depth %>%
  summarise(
    mean = mean(depth_diff, na.rm = TRUE),
    min = min(depth_diff, na.rm = TRUE),
    max = max(depth_diff, na.rm = TRUE),
    sd = round(sd(depth_diff, na.rm = TRUE), digits = 2)
  ) %>%
  mutate(threshold = round(mean + 3 * sd, digits = 2))

stats <- dat_depth %>%
  group_by(county) %>%
  summarise(
    mean = mean(depth_diff, na.rm = TRUE),
    min = min(depth_diff, na.rm = TRUE),
    max = max(depth_diff, na.rm = TRUE),
    sd = round(sd(depth_diff, na.rm = TRUE), digits = 2)
  ) %>%
  mutate(threshold = round(mean + 3 * sd, digits = 2))

  #mutate(threshold = round(max, digits = 2))


# export thresholds -------------------------------------------------------

roc_depth_threshold <- stats %>%
  select(county, threshold_value = threshold) %>%
  mutate(threshold = "sensor_depth_rate_of_change")

# Export rds file
saveRDS(roc_depth_threshold, file = here("inst/data/roc_depth_thresholds.RDS"))




