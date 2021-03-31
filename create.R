library('devtools')
library(roxygen2)
library(dplyr)
library(tidyr)
library(testthat)
library(knitr)

# library(devtools)
# has_devel()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_r_pkg")
# devtools::create("MEPS")
#
setwd("./MEPS")

# Use Yihui's library to tidy code
#formatR::tidy_dir("R")

# Create documentation --------------------------------------------------------
devtools::document()


# Test ------------------------------------------------------------------------
install("MEPS")
library(MEPS)
fyc17 <- read_MEPS(year = 2017, type = "FYC")
head(fyc17)

cond17 <- read_MEPS(year = 2017, type = "Conditions")
head(cond17)

cond18 <- read_MEPS(year = 2018, type = "Conditions")
head(cond18)
