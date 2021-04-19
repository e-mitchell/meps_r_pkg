library(devtools)
library(roxygen2)
library(testthat)
#library(knitr)

# Import packages -------------------------------------------------------------
# use_package("dplyr")
# use_package("tidyr")
# use_package("haven")
# use_package("readxl")
# use_package("stringr")
# use_package("httr")
# use_package("foreign")
# use_package("readr")
# use_package("tibble")
# use_package("magrittr")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# devtools::create("MEPS")

setwd("MEPS")


# Run after updating ----------------------------------------------------------

load_all() # use 'load_all()' to import all functions in package in current state

document() # Create documentation
# ?read_MEPS # check documentation

check() # make sure everything looks good. Spoiler alert!! It does not.


install()




# Test ------------------------------------------------------------------------

# create test files (run once)
# use_test("read_MEPS")
# use_test("get_puf_names")

test()

# Other helpful functions (from Hadley book) ----------------------------------
#
# use_r: Make new skeleton function files
# use_readme_rmd(): Make a new README file


