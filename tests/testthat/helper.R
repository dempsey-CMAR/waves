
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
dat_qc <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_test_grossrange(
    county = "Halifax",
    first_pivot_col = 3,
    last_pivot_col = 12
  ) %>%
  wv_pivot_vars_longer(
    first_pivot_col = 3,
    last_pivot_col = 12) %>%
  wv_pivot_flags_longer()

dat_qc_4 <- dat_qc %>%
  filter(
    (day_utc == 1 &
       (variable == "significant_height_m" | variable == "peak_period_s")) |

      (day_utc == 2 & (variable == "average_height_largest_33_percent_m" |
                         variable == "period_largest_33_percent_s")) |

      (day_utc == 3 & (variable == "average_height_largest_10_percent_m" |
                         variable == "period_largest_10_percent_s")) |

      (day_utc == 4 & (variable == "maximum_height_m" |
                         variable == "period_maximum_s")) |

      ((day_utc == 5 | day_utc == 6) & variable == "to_direction_degree")
  )


dat_qc_1 <- dat_qc %>%
  dplyr::anti_join(
    dat_qc_4,
    by = dplyr::join_by(
      timestamp_utc, day_utc, variable, value, grossrange_flag_value
    )
  )

# trim depth --------------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
dat_trim <- readRDS(paste0(path, "/wv_test_data_grossrange.RDS")) %>%
  wv_flag_sensor_depth_to_trim(return_depth_diff = TRUE)

dat_trim_4 <- dat_trim %>%
  dplyr::slice(c(1, 2, nrow(dat_trim) - 1, nrow(dat_trim)))
