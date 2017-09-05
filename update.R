library(dplyr)

setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_r_pkg")

# Update saved data PUF names

# Load latest PUF names from GitHub

meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

puf_names_current = read.csv(meps_file,stringsAsFactors = F)

puf_names <- puf_names_current %>%
  mutate(Year = suppressWarnings(as.numeric(Year))) %>%
  filter(!is.na(Year))

event_letters <- list(DV="b",OM="c",IP="d",ER="e",OP="f",OB="g",HH="h")
hc_list <- c("h10a","h26bf1",
             sprintf("h16%sf1",letters[2:8]),
             sprintf("h10%sf1",letters[2:8]))

meps_names <- puf_names %>% 
  rename(RX=RX.Events) %>%
  mutate(RX = replace(RX,RX=="h10a","hc10a"),
         CLNK = replace(CLNK,CLNK=="h10if1","hc10if1"),
         Conditions = replace(Conditions,Conditions=="h06r","hc006r"))

for(evnt in names(event_letters)){
  letter = event_letters[[evnt]]
  value = meps_names$Events %>% gsub("\\*",letter,.)
  special = value %in% hc_list
  value[special] = value[special] %>% sub("h","hc",.)
  meps_names[,evnt] = value
}

meps_names = meps_names %>% select(-Events)

devtools::use_data(meps_names,pkg="MEPS",overwrite=T)
