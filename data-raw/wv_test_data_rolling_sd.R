# October 2, 2024

# Simulate data for the rolling standard deviation test

# Day 3: Rolling Standard Deviation 3 (high)


library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime day
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# this will work better in a loop over variables
# Raw data ----------------------------------------------------------------
path <- here("inst/testdata")

stats <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt") %>%
  wv_assign_cf_variable_names() %>%
  wv_assign_short_variable_names() %>%
  wv_pivot_vars_longer() %>%
  group_by(variable) %>%
  summarise(mean = round(mean(value), digits = 3))

roll_sd <- wv_thresholds %>%
  filter(county == "Halifax" | is.na(county), threshold == "rolling_sd_max") %>%
  select(variable, threshold_value)

vars <- unique(roll_sd$variable)

# simulate data
timestamp_utc = seq(
  as_datetime("2023-01-01 12:00:00"), as_datetime("2023-01-07 12:00:00"),
  by = paste0(60, " mins")
)

dat <- expand.grid(variable = vars, timestamp_utc = timestamp_utc) %>%
  left_join(roll_sd, by = "variable") %>%
  mutate(day_utc = day(timestamp_utc))

attr(dat, "out.attrs") <- NULL

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


dat_sd %>%
  wv_test_rolling_sd(county = "Halifax") %>%
  wv_pivot_flags_longer(qc_tests = "rolling_sd") %>%
  wv_plot_flags(qc_tests = "rolling_sd")


# Export rds file
saveRDS(dat_sd, file = here("inst/testdata/wv_test_data_rolling_sd.RDS"))
