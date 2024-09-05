test_that("wv_test_grossrange_all_vars() assigns correct flags", {
  expect_equal(as.numeric(unique(qc_gr_1$grossrange_flag_value)), 1)

  expect_equal(as.numeric(unique(qc_gr_3$grossrange_flag_value)), 3)

  expect_equal(as.numeric(unique(qc_gr_4$grossrange_flag_value)), 4)

})
