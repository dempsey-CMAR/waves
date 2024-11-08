test_that("wv_test_rolling_sd() assigns correct flags", {
  expect_equal(nrow(dat_all), nrow(dat_all_qc))

  # will need to adjust number of columns as tests and metadata are added
  expect_equal(ncol(dat_all_qc), 52)

})
