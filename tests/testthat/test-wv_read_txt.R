test_that("wv_read_txt() reads in all columns", {
  expect_equal(
    colnames(dat1),
    c("timestamp_utc", "Hs", "H1/10", "H1/3", "Hmax",
      "Tp", "T1/10", "T1/3", "Tmax",
      "Dp", "Depth", "CM", "CD"
    )
  )
})

test_that("wv_read_txt() reads in all rows", {
  expect_equal(nrow(dat1), 102)
})
