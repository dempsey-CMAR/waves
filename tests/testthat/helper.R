
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
  wv_pivot_vars_longer(first_pivot_col = "significant_height_m")

dat3_2 <- dat5 %>%
  pivot_wider(names_from = variable, values_from = value)

# short var names
# check that first_pivot_col works with column number
dat6 <- dat4 %>%
  wv_pivot_vars_longer(first_pivot_col = 2)

dat4_2 <- dat6 %>%
  pivot_wider(names_from = variable, values_from = value)


# grossrange test ---------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
dat_gr <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_test_grossrange(
    county = "Halifax",
    first_pivot_col = 3,
    last_pivot_col = 12
  ) %>%
  wv_pivot_vars_longer(
    first_pivot_col = 3,
    last_pivot_col = 12) %>%
  wv_pivot_flags_longer()

# note: user_min = gr_min, so obs on day 4 are flagged Fail (not suspect)
qc_gr_4 <- dat_gr %>%
  filter(day_utc %in% c(2, 4, 6))

qc_gr_3 <- dat_gr %>%
  filter(day_utc == 8)

qc_gr_1 <- dat_gr %>%
  dplyr::anti_join(
    qc_gr_4,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, grossrange_flag_value
    )
  ) %>%
  dplyr::anti_join(
    qc_gr_3,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, grossrange_flag_value
    ))

# trim depth --------------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
dat_trim <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_flag_sensor_depth_to_trim(return_depth_diff = TRUE)

dat_trim_4 <- dat_trim %>%
  dplyr::slice(c(1, 2, nrow(dat_trim) - 1, nrow(dat_trim)))
