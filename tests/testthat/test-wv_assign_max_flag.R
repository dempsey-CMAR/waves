test_that("wv_assign_max_flag() assigns correct flag value", {
  expect_equal(
    dat_max_flag$qc_flag_significant_wave_height_m,
    ordered( c(1, 2, 3, 4, 4), levels = 1:4)
    )
})
