library('devtools')
library(roxygen2)

setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_r_pkg")
# devtools::create("MEPS")
# 
setwd("./MEPS")

# Use Yihui's library to tidy code
formatR::tidy_dir("R")

# Create documentation
devtools::document()


# test
setwd("..")
install("MEPS")
library(MEPS)
