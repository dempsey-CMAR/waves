test_that("wv_assign_cf_variable_names() assigns correct names()", {
  expect_equal(
    colnames(dat2),
    c("timestamp_utc",
      "sea_surface_wave_significant_height_m",
      "sea_surface_wave_average_height_largest_10_percent_m",
      "sea_surface_wave_average_height_largest_33_percent_m",
      "sea_surface_wave_maximum_height_m",
      "sea_surface_wave_peak_period_s",
      "sea_surface_wave_period_largest_10_percent_s",
      "sea_surface_wave_period_largest_33_percent_s",
      "sea_surface_wave_period_maximum_s",
      "sea_surface_wave_from_direction_degree",
      "sensor_depth_below_surface_m",
      "sea_water_speed_m_s",
      "sea_water_to_direction_degree")
  )
})

test_that("wv_assign_short_variable_names() assigns correct names", {

  expect_false(any(str_detect(colnames(dat3), "sea_surface_wave_")))

})

test_that("wv_append_long_variable_names() assigns correct names", {

  expect_equal(colnames(dat2), colnames(dat4))

})
