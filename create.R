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
meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"
puf_names_current <- utils::read.csv(meps_file, stringsAsFactors = F)

meps_long_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_longitudinal_file_names.csv"
long_names_current <- utils::read.csv(meps_long_file, stringsAsFactors = F)


# or this:
# puf_names_current  = read.csv("../../hhs_ahrq/MEPS/Quick_Reference_Guides/meps_file_names.csv")
# long_names_current = read.csv("../../hhs_ahrq/MEPS/Quick_Reference_Guides/meps_longitudinal_file_names.csv")


puf_names_cached <- puf_names_current %>%
  dplyr::mutate(Year = suppressWarnings(as.numeric(Year))) %>%
  dplyr::filter(!is.na(Year))

long_names_cached <- long_names_current %>% 
  dplyr::filter(File_Name != "")

usethis::use_data(puf_names_cached, internal = T, overwrite = T)
usethis::use_data(long_names_cached, internal = T, overwrite = T)



# Run after updating ----------------------------------------------------------

load_all() # use 'load_all()' to import all functions in package in current state

document() # Create documentation
?read_MEPS # check documentation
?get_puf_names


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

# asking for Longitudinal data with year should give error
get_puf_names(year = 2020, type = "LONG")


get_puf_names()

get_puf_names(year = 2022)
get_puf_names(type = "op")
get_puf_names(type = "long")

get_puf_names(type = "long", panel = 15)
get_puf_names(type = "long", panel = 23)
get_puf_names(type = "long", panel = 24)

# Testing 3-year and 4-year files for Panels 23, 24
get_puf_names(type = "long", panel = 23, long_type = "3-year")
get_puf_names(type = "long", panel = 23, long_type = "4-year")

get_puf_names(type = "long", panel = 24, long_type = "3-year")
get_puf_names(type = "long", panel = 24, long_type = "4-year")



get_puf_names(year = 2018, type = "IP")
get_puf_names(year = 2016:2018, type = "cond")
get_puf_names(year = c(2000, 2004,2018), type = c("FYC", "Inpatient"))

get_puf_names(type = "brr")
get_puf_names(type = "pooled linkage")



# These should work
read_MEPS(year = 2020, type = "OP")
read_MEPS(panel = 24, type = "Longitudinal")
read_MEPS(panel = 24, type = "Longitudinal", long_type = "3-year")

read_MEPS(type = "BRR")
read_MEPS(type = "PL")

read_MEPS(file = "h234")



# ERRORS: Out of scope
read_MEPS(year = 2019, type = "OX")
read_MEPS(year = 2030, type = "OP")

# ERRORS: 'too many files' requested
read_MEPS(year = 2019:2021, type = "IP") 
read_MEPS(year = 2019, type = c("IP", "ER")) 
read_MEPS(year = 2019) 
read_MEPS(type = "IP") 
read_MEPS(file = c("h234","h235"))





# Other helpful functions (from Hadley book) ----------------------------------
#
# use_r: Make new skeleton function files
# use_readme_rmd(): Make a new README file


