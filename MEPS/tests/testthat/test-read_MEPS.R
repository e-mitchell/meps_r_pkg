test_that("Requesting multiple years/files/types throws an error", {
  expect_error(read_MEPS(year = 2001:2002, type = "OB"))
  expect_error(read_MEPS(year = 2020, type = c("OB", "OP")))
  expect_error(read_MEPS(file = c("h211", "h222")))
})


test_that("Must include file OR (year AND type)", {
  expect_error(read_MEPS(year = 2019))
  expect_error(read_MEPS(type = "COND"))
})



test_that("If both file AND (year OR type), warn that file is used", {
  expect_warning(read_MEPS(file = "H222", year = 2019))
})

