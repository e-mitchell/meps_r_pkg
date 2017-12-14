library('devtools')
library(roxygen2)
library(dplyr)
library(tidyr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_r_pkg")
# devtools::create("MEPS")
# 
setwd("./MEPS")

# Use Yihui's library to tidy code
formatR::tidy_dir("R")

# Create documentation --------------------------------------------------------
devtools::document()

# Add collapsed condition codes data to package -------------------------------
setwd("..")

cnd_codes <- read.csv("../MEPS/Quick_Reference_Guides/meps_condition_codes.csv")

condition_codes <- cnd_codes %>% 
  separate_rows(CCS.Codes,sep=",") %>%
  separate(CCS.Codes,sep="-",c("min","max"),fill='right') %>%
  mutate(max = ifelse(is.na(max),min,max)) %>% 
  rowwise %>%
  mutate(CCS_Codes = paste0(min:max,collapse=",")) %>% 
  ungroup %>%
  separate_rows(CCS_Codes,sep=",") %>%
  mutate(CCS_Codes = as.numeric(as.character(CCS_Codes))) %>%
  select(Condition, CCS_Codes)

devtools::use_data(condition_codes, pkg = "MEPS", overwrite = F)

# Add TC1 codes data to packages ----------------------------------------------

tc1_names <- read.csv("meps_tc1_names.csv")

devtools::use_data(tc1_names, pkg = "MEPS", overwrite = F)



# Test ------------------------------------------------------------------------
install("MEPS")
library(MEPS)
