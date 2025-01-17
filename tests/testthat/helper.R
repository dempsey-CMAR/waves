
# Common compile arguments ------------------------------------------------

path <- system.file("testdata", package = "waves")

dat1 <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt")

# long and short variable names ------------------------------------------------------------

dat2 <- dat1 %>% wv_assign_cf_variable_names()

dat3 <- dat2 %>% wv_assign_short_variable_names()

dat4 <- dat3 %>%
  wv_append_long_variable_names()


# pivoting ----------------------------------------------------------------

# long var names
# check that first_pivot_col works with column name
dat5 <- dat3 %>%
  wv_pivot_vars_longer()

dat3_2 <- dat5 %>%
  pivot_wider(names_from = variable, values_from = value)

# short var names
# check that first_pivot_col works with column number
# dat6 <- dat4 %>%
#   wv_pivot_vars_longer(first_pivot_col = 2)

# dat4_2 <- dat6 %>%
#   pivot_wider(names_from = variable, values_from = value)


# grossrange test ---------------------------------------------------------

height_vars <- c("significant_height_m", "average_height_largest_33_percent_m",
                 "average_height_largest_10_percent_m", "maximum_height_m")
period_vars <- c("peak_period_s", "period_largest_33_percent_s",
                 "period_largest_10_percent_s", "period_maximum_s")

dat <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS"))

dat_gr <- dat %>%
  wv_test_grossrange(county = "Halifax") %>%
  wv_pivot_flags_longer(qc_tests = "grossrange")

p <- wv_plot_flags(
  dat_gr, qc_tests = "grossrange",
  n_col = 2)

# note: user_min = gr_min, so obs on day 4 are flagged Fail (not suspect)
gr_4 <- dat_gr %>%
  filter(
    day_utc %in% c(2, 4, 6),
    # these are now flagged by the crossref test NOT grossrange
    !(variable %in% height_vars & day_utc == 2)
  )

# the direction variables cannot have flag of 3, since gr_max = user_max
# and gr_min = user_min
gr_3 <- dat_gr %>%
  filter(day_utc == 8 & !str_detect(variable, "direction"))

gr_1 <- dat_gr %>%
  dplyr::anti_join(
    gr_4,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, grossrange_flag_value
    )
  ) %>%
  dplyr::anti_join(
    gr_3,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, grossrange_flag_value
    ))



# crossref check ----------------------------------------------------------
dat_cc <- dat %>%
  wv_test_crossref() %>%
  wv_pivot_flags_longer(qc_tests =  "crossref")

cc_4 <- dat_cc %>%
  filter(
    variable %in% c(height_vars, period_vars) & (day_utc %in% c(2, 4))
  )
cc_1 <- dat_cc %>%
  dplyr::anti_join(
    cc_4,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, crossref_flag_value
    )
  )

# rolling_sd --------------------------------------------------------------
dat_rolling_sd <- readRDS(paste0(path, "/wv_test_data_rolling_sd.RDS")) %>%
  wv_test_rolling_sd(county = "Halifax") %>%
  wv_pivot_flags_longer(qc_tests = "rolling_sd")

rolling_sd_3 <- dat_rolling_sd %>%
  filter(
    timestamp_utc >= as_datetime("2023-01-02 20:00:00") &
      timestamp_utc <= as_datetime("2023-01-04 04:00:00")
  )

rolling_sd_2 <- dat_rolling_sd %>%
  filter(
    timestamp_utc <= as_datetime("2023-01-01 22:00:00") |
      timestamp_utc >= as_datetime("2023-01-07 01:00:00")
  )

rolling_sd_1 <- dat_rolling_sd %>%
  dplyr::anti_join(
    rolling_sd_3,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, rolling_sd_flag_value
    )
  ) %>%
  dplyr::anti_join(
    rolling_sd_2,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, rolling_sd_flag_value
    ))

# Spike -------------------------------------------------------------------

dat_spike <- readRDS(paste0(path, "/wv_test_data_spike.RDS")) %>%
  wv_test_spike(county = "Yarmouth") %>%
  wv_pivot_flags_longer(qc_tests = "spike")

spike_2 <- dat_spike %>%
  group_by(variable) %>%
  filter(row_number() == 1 | row_number() == n())

spike_3 <- dat_spike %>%
  filter(
    (day_utc == 3 & hour_utc == 0) |
      (day_utc == 3 & hour_utc == 12) |
      (day_utc == 4 & hour_utc == 23) |
      (day_utc == 5 & hour_utc == 1) |
      (day_utc == 5 & hour_utc == 11) |
      (day_utc == 5 & hour_utc == 13)
  )

spike_4 <- dat_spike %>%
  filter(
    (day_utc == 5 & hour_utc == 0) |
      (day_utc == 5 & hour_utc == 12)
  )

spike_1 <- dat_spike %>%
  dplyr::anti_join(
    spike_2,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, spike_flag_value
    )
  ) %>%
  dplyr::anti_join(
    spike_3,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, spike_flag_value
    )
  ) %>%
  dplyr::anti_join(
    spike_4,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, spike_flag_value
    ))

# wv_test_all -------------------------------------------------------------

dat_all <- dat_rolling_sd %>%
  select(-c(rolling_sd_flag_value, day_utc)) %>%
  pivot_wider(values_from = "value", names_from = "variable")

dat_all_qc <- dat_all %>%
  wv_test_all(county = "Halifax")


# Assign max flag ---------------------------------------------------------

dat_max_flag <- data.frame(
  timestamp_utc = c("a", "b", "c", "d", "e"),
  variable = "significant_wave_height_m",
  value = round(c(runif(5)), digits = 2),
  grossrange_flag_value = c(1, 0, 2, 3, 1),
  rolling_sd_flag_value = c(0, 2, 1, 3, 1),
  spike_flag_value = c(1, 1, 3, 4, 4)
) %>%
  wv_assign_max_flag()

# trim depth --------------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
dat_trim <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_start_end_obs_to_trim(return_depth_diff = TRUE)
  #wv_flag_sensor_depth_to_trim(return_depth_diff = TRUE)

dat_trim_4 <- dat_trim %>%
  dplyr::slice(c(1, 2, nrow(dat_trim) - 1, nrow(dat_trim)))

