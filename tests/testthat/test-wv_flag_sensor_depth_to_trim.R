test_that("wv_flag_sensor_depth_to_trim() assigns correct flags", {
  expect_equal(as.numeric(unique(dat_trim_4$depth_trim_flag)), 4)
})
