library(dplyr)

# Update saved data PUF names

setwd("C:/Users/emily.mitchell/Desktop/GitHub/meps_r_pkg")

# Load latest PUF names from GitHub -------------------------------------------

meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

# meps_file = "C:/Users/emily.mitchell/Desktop/GitHub/hhs_ahrq/MEPS/Quick_Reference_Guides/meps_file_names.csv"

puf_names_current <- read.csv(meps_file, stringsAsFactors = F)

puf_names <- puf_names_current %>%
  mutate(Year = suppressWarnings(as.numeric(Year))) %>%
  filter(!is.na(Year))

# Expand event file names -----------------------------------------------------

meps_names <- puf_names %>% rename(RX=RX.Events) 
event_letters <- list(DV="b",OM="c",IP="d",ER="e",OP="f",OB="g",HH="h")

for(evnt in names(event_letters)){
  letter = event_letters[[evnt]]
  value = meps_names$Events %>% gsub("\\*",letter,.)
  meps_names[,evnt] = value
}

meps_names = meps_names %>% select(-Events)

devtools::use_data(meps_names, pkg = "MEPS", overwrite = T)
