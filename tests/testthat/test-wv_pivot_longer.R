test_that("wv_pivot_longer() can be converted back to dat_wide", {
  expect_equal(tidyr::as_tibble(dat3), dat3_2)
  expect_equal(tidyr::as_tibble(dat4), dat4_2)
})
