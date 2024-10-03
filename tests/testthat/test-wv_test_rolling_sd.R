test_that("wv_test_rolling_sd() assigns correct flags", {
  expect_equal(as.numeric(unique(rolling_sd_1$rolling_sd_flag_value)), 1)

  expect_equal(as.numeric(unique(rolling_sd_2$rolling_sd_flag_value)), 2)

  expect_equal(as.numeric(unique(rolling_sd_3$rolling_sd_flag_value)), 3)

})
