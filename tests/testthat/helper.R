
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

dat_gr <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_test_grossrange(county = "Halifax") %>%
  wv_pivot_flags_longer(qc_tests = "grossrange")

p <- wv_plot_flags(
  dat_gr, qc_tests = "grossrange",
  n_col = 2)

# note: user_min = gr_min, so obs on day 4 are flagged Fail (not suspect)
gr_4 <- dat_gr %>%
  filter(day_utc %in% c(2, 4, 6))

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

# wv_test_all -------------------------------------------------------------

dat_all <- dat_rolling_sd %>%
  select(-rolling_sd_flag_value) %>%
  pivot_wider(values_from = "value", names_from = "variable")

dat_all_qc <- dat_all %>%
  wv_test_all(county = "Halifax")






# trim depth --------------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
dat_trim <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_flag_sensor_depth_to_trim(return_depth_diff = TRUE)

dat_trim_4 <- dat_trim %>%
  dplyr::slice(c(1, 2, nrow(dat_trim) - 1, nrow(dat_trim)))

