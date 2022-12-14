# Load dev packages -----------------------------------------------------------
library(devtools)
library(roxygen2)
library(testthat)
#library(knitr)

# Import packages for MEPS functions ------------------------------------------
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
# use_package("curl")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# devtools::create("MEPS")

setwd("MEPS")


# Add/update meps_puf_names in case internet connection is unavailable --------
# meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"
# puf_names_current <- utils::read.csv(meps_file, stringsAsFactors = F)

# # or this: puf_names_current = read.csv("../../hhs_ahrq/MEPS/Quick_Reference_Guides/meps_file_names.csv")

puf_names_cached <- puf_names_current %>%
  dplyr::mutate(Year = suppressWarnings(as.numeric(Year))) %>%
  dplyr::filter(!is.na(Year))

usethis::use_data(puf_names_cached, internal = T, overwrite = T)

# Run after updating ----------------------------------------------------------

load_all() # use 'load_all()' to import all functions in package in current state

document() # Create documentation
# ?read_MEPS # check documentation
# ?get_puf_names


check() # make sure everything looks good. Spoiler alert!! It does not.


install()


# Optional: Update version number ---------------------------------------------

# usethis::use_version()



# Testing ---------------------------------------------------------------------

# create test files (run once)
# use_test("read_MEPS")
# use_test("get_puf_names")

test()

# additional tests to just see what functions do:

# Test this manually, since it will change each year:

#  - As of 12/13/2022: asking for 2021 data should give an error
get_puf_names(year = 2021, type = "LONG")

# asking for 2020 Longitudinal data should give message
get_puf_names(year = 2020, type = "LONG")
get_puf_names(year = 2019:2020, type = "long")



get_puf_names()

get_puf_names(year = 2018)
get_puf_names(type = "op")
get_puf_names(type = "long")

get_puf_names(year = 2018, type = "IP")
get_puf_names(year = 2016:2018, type = "cond")
get_puf_names(year = c(2000, 2004,2018), type = c("FYC", "Inpatient"))

get_puf_names(type = "brr")
get_puf_names(type = "pooled linkage")



read_MEPS(year = 2020, type = "Long") %>% head

read_MEPS(year = 2014, type = "OM") %>% head
read_MEPS(year = 2017, type = "OM") %>% head
read_MEPS(year = 2019, type = "OM") %>% head

read_MEPS(type = "BRR") %>% head
read_MEPS(type = "pooled linkage") %>% head

read_MEPS("h36brr19") %>% head
read_MEPS("h36u19") %>% head


# Other helpful functions (from Hadley book) ----------------------------------
#
# use_r: Make new skeleton function files
# use_readme_rmd(): Make a new README file


