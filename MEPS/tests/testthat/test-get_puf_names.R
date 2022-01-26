

test_that("Specifying only 'year' returns vector of files for that year", {
  expect_equal(
    get_puf_names(year = 2011)[1:7],
    data.frame(YEAR = 2011, PIT = "h134", FYC = "h147", CONDITIONS = "h146",
               PMED = "h144a", JOBS = "h142", PRPL = "h145"))

  expect_equal(get_puf_names(year = 1996)[1], data.frame(YEAR = 1996))
  expect_equal(get_puf_names(year = 2000)[1], data.frame(YEAR = 2000))
  expect_equal(get_puf_names(year = 2002)[1], data.frame(YEAR = 2002))
  expect_equal(get_puf_names(year = 2018)[1], data.frame(YEAR = 2018))

})


test_that("Specifying only 'type' returns columns vector of files for that type", {
  expect_equal(
    get_puf_names(type = "PIT")[1:5,],
    data.frame(YEAR = 1996:2000, PIT = c("h01", "h05", "h09", "h13","h22")))

  expect_equal(
    get_puf_names(type = "FYC")[1:5,],
    data.frame(YEAR = 1996:2000, FYC = c("h12", "h20", "h28", "h38", "h50")))

  expect_equal(
    get_puf_names(type = "RX")[1:5,],
    data.frame(YEAR = 1996:2000, RX = c("h10a", "h16a", "h26a", "h33a", "h51a")))
})

test_that("Missing 'type' and 'year' returns full data frame", {

  load("snapshots/get-puf-names_1996_2017.Rdata")
  expect_equal(
    get_puf_names()[1:22,],
    snapshot_1996_2017
   )
})

test_that("Alternate abbreviations are equivalent (e.g. MV/OB, IP/HS, PRP/PRPL)", {
  expect_equal(
    unlist(get_puf_names(year = 2011, type = "MV")) %>% setNames(NULL),
    unlist(get_puf_names(year = 2011, type = "OB")) %>% setNames(NULL))

  expect_equal(
    unlist(get_puf_names(year = 2011, type = "IP")) %>% setNames(NULL),
    unlist(get_puf_names(year = 2011, type = "HS")) %>% setNames(NULL))

  expect_equal(
    unlist(get_puf_names(year = 2011, type = "PRP")) %>% setNames(NULL),
    unlist(get_puf_names(year = 2011, type = "PRPL")) %>% setNames(NULL))
})


test_that("Specifying 'type' and 'year' gives single result", {
  expect_equal(get_puf_names(year = 2011, type = "PIT"), data.frame(PIT = "h134"))
  expect_equal(get_puf_names(year = 2011, type = "FYC"), data.frame(FYC = "h147"))
  expect_equal(get_puf_names(year = 2011, type = "CONDITIONS"), data.frame(CONDITIONS = "h146"))
  expect_equal(get_puf_names(year = 2011, type = "PMED"), data.frame(PMED = "h144a"))

  expect_equal(get_puf_names(year = 1996, type = "DV"),  data.frame(DV = "h10bf1"))
  expect_equal(get_puf_names(year = 1996, type = "RX"), data.frame(RX = "h10a"))

  expect_equal(get_puf_names(year = 2000, type = "JOBS"), data.frame(JOBS = "h40"))
  expect_equal(get_puf_names(year = 2000, type = "PRPL"), data.frame(PRPL = "h47f4"))

  expect_equal(get_puf_names(year = 2002, type = "OB"), data.frame(OB = "h67g"))
  expect_equal(get_puf_names(year = 2002, type = "MV"), data.frame(MV = "h67g"))
})


test_that("Sentence case doesn't matter", {
  expect_equal(get_puf_names(year = 2011, type = "pit"), data.frame(PIT = "h134"))
  expect_equal(get_puf_names(year = 2011, type = "fYC"), data.frame(FYC = "h147"))
  expect_equal(get_puf_names(year = 2011, type = "conditions"), data.frame(CONDITIONS = "h146"))
  expect_equal(get_puf_names(year = 2011, type = "pMED"), data.frame(PMED = "h144a"))

  expect_equal(get_puf_names(year = 1996, type = "dV"),  data.frame(DV = "h10bf1"))
  expect_equal(get_puf_names(year = 1996, type = "rX"), data.frame(RX = "h10a"))

  expect_equal(get_puf_names(year = 2000, type = "jOBS"), data.frame(JOBS = "h40"))
  expect_equal(get_puf_names(year = 2000, type = "pRPL"), data.frame(PRPL = "h47f4"))

  expect_equal(get_puf_names(year = 2002, type = "oB"), data.frame(OB = "h67g"))
  expect_equal(get_puf_names(year = 2002, type = "mV"), data.frame(MV = "h67g"))
})

# Expected ERRORs -------------------------------------------------------------

test_that("ERROR: year outside scope", {
  expect_error(get_puf_names(year = 1995))
  expect_error(get_puf_names(year = 1997.5))
  expect_error(get_puf_names(year = "1995"))
  expect_error(get_puf_names(year = 2500))
  expect_error(get_puf_names(year = -2))
  expect_error(get_puf_names(year = 0))
  expect_error(get_puf_names(year = "hi"))

  expect_error(get_puf_names(year = 1995, type = "RX"))
  expect_error(get_puf_names(year = 1997.5, type = "FYC"))
  expect_error(get_puf_names(year = "1995", type = "PIT"))
  expect_error(get_puf_names(year = 2500, type = "JOBS"))
  expect_error(get_puf_names(year = -2, type = "CONDITIONS"))
  expect_error(get_puf_names(year = 0, type = "PMED"))
  expect_error(get_puf_names(year = "hi", type = "MV"))
})

test_that("ERROR: wrong file name", {
  expect_error(get_puf_names(year = 1996, type = "put"))
  expect_error(get_puf_names(year = 1996, type = "emily"))
  expect_error(get_puf_names(year = 1996, type = 5))

  expect_error(get_puf_names(type = "no"))
  expect_error(get_puf_names(type = "FANCY"))
  expect_error(get_puf_names(type = 30))
})
