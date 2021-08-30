

test_that("'year' only returns vector of files for that year", {
  expect_equal(
    get_puf_names(year = 2011)[1:7],
    data.frame(YEAR = 2011, PIT = "h134", FYC = "h147", CONDITIONS = "h146",
               PMED = "h144a", JOBS = "h142", PRPL = "h145"))
})
