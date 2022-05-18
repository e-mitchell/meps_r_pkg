

test_that("Specifying only 'year' returns vector of files for that year", {
  expect_equal(
    get_puf_names(year = 2011)[1:7],
    data.frame(YEAR = 2011, PIT = "h134", FYC = "h147", CONDITIONS = "h146",
               PMED = "h144a", JOBS = "h142", PRPL = "h145"))

  expect_equal(get_puf_names(year = 1996)[1], data.frame(YEAR = 1996))
  expect_equal(get_puf_names(year = 2018)[1], data.frame(YEAR = 2018))

})


test_that("Specifying only 'type' returns columns vector of files for that type", {
  expect_equal(
    get_puf_names(type = "PIT")[1:5,],
    data.frame(YEAR = 1996:2000, PIT = c("h01", "h05", "h09", "h13","h22")))

  expect_equal(
    get_puf_names(type = "RX")[1:5,],
    data.frame(YEAR = 1996:2000, RX = c("h10a", "h16a", "h26a", "h33a", "h51a")))
})


test_that("Multiple 'types' and 'years' gives vector of files for each type, year", {

  expect_equal(
    get_puf_names(type = c("FYC", "RX", "JOBS"))[1:5,],
    data.frame(YEAR = 1996:2000,
               FYC = c("h12", "h20", "h28", "h38", "h50"),
               RX = c("h10a", "h16a", "h26a", "h33a", "h51a"),
               JOBS = c("h07", "h19", "h25", "h32", "h40")))

  expect_equal(
    get_puf_names(year = 2015:2018)[,1:3],
    data.frame(YEAR = 2015:2018,
               PIT = c("h167", "h177", "h186", "h196"),
               FYC = c("h181", "h192", "h201", "h209")))

  expect_equal(
    get_puf_names(year = c(1996, 2000, 2002, 2018), type = c("PIT", "FYC")),
    data.frame(YEAR = c(1996, 2000, 2002, 2018),
               PIT = c("h01", "h22", "h53", "h196"),
               FYC = c("h12", "h50", "h70", "h209")))
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

  expect_equal(
    unlist(get_puf_names(year = 2011, type = "COND")) %>% setNames(NULL),
    unlist(get_puf_names(year = 2011, type = "CONDITIONS")) %>% setNames(NULL))

  expect_equal(
    unlist(get_puf_names(year = 2011, type = "CONDITION")) %>% setNames(NULL),
    unlist(get_puf_names(year = 2011, type = "CONDITIONS")) %>% setNames(NULL))
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



test_that("Multiple 'types' gives an error when 'BRR' or 'Pooled linkage' are chosen", {
  expect_error(get_puf_names(type = c("BRR", "FYC")))
  expect_error(get_puf_names(type = c("PL", "OM")))
  expect_error(get_puf_names(type = c("Pooled Linkage", "OM")))
})

test_that("ERROR: year outside scope", {
  expect_error(get_puf_names(year = 1995:1997))
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

  expect_error(get_puf_names(type = c("FYC", "hot")))
  expect_error(get_puf_names(year = 2002, type = c(30, "FYC")))
})

test_that("ERROR: specifying type = 'year' or 'panel' or 'new.panel'", {
  expect_error(get_puf_names(year = 1996, type = "year"))
  expect_error(get_puf_names(year = 1996, type = "old.panel"))
  expect_error(get_puf_names(year = 1996, type = "new.panel"))
  expect_error(get_puf_names(year = 1996, type = "PANELS"))

  expect_error(get_puf_names(type = "year"))
  expect_error(get_puf_names(type = "old.panel"))
  expect_error(get_puf_names(type = "new.panel"))
  expect_error(get_puf_names(type = "panels"))

  expect_error(get_puf_names(type = "YEAR"))
  expect_error(get_puf_names(type = "OLD.PANEL"))
  expect_error(get_puf_names(type = "NEW.PANEL"))
  expect_error(get_puf_names(type = "PANELS"))
})
