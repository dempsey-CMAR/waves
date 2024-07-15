
# Common compile arguments ------------------------------------------------

path <- system.file("testdata", package = "waves")


dat1 <- wv_read_txt(path, "2022-09-29_western_shoal_test_data.txt")


# long and short variable names ------------------------------------------------------------

dat2 <- dat1 %>% wv_assign_cf_variable_names()

dat3 <- dat2 %>% wv_assign_short_variable_names()

dat4 <- dat3 %>%
  wv_append_long_variable_names()


# grossrange test ---------------------------------------------------------

# export dat_qc from wv_test_Data_grossrange
# dat_qc <- dat %>%
#   wv_test_grossrange_all_vars() %>%
#   wv_pivot_vars_longer(
#     first_pivot_col = 3,
#     last_pivot_col = 11)  %>%
#   wv_pivot_flags_longer()

# dat_qc_4 <- dat_qc %>%
#   filter(
#     (day_utc == 1 &
#       (variable == "significant_height_m" | variable == "peak_period_s")) |
#     (day_utc == 2 & (variable == "average_height_largest_33_percent_m" |
#        variable == "period_largest_33_percent_s"))
#   )
#


