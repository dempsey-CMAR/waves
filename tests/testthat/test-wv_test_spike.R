test_that("wv_test_spike() assigns correct flags", {
  expect_equal(as.numeric(unique(spike_1$spike_flag_value)), 1)

  expect_equal(as.numeric(unique(spike_2$spike_flag_value)), 2)

  expect_equal(as.numeric(unique(spike_3$spike_flag_value)), 3)

  expect_equal(as.numeric(unique(spike_4$spike_flag_value)), 4)

})
