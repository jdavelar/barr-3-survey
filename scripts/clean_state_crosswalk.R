################################################
#### CREATE LONG VERSION OF STATE CROSSWALK ####
################################################

library(here)
library(rio)
library(tidyverse)

dat <- import(here("data/Across states", "state_crosswalk_combined.csv"))

long_dat <- dat %>% 
  janitor::clean_names() %>% 
  select(-c(supt_id, i_ds_will_be_added_late_sept_early_oct)) %>% #don't forget to remove line once IDs are added
  separate_rows(district, sep = "/")

write.csv(long_dat, "data/Across states/state_crosswalk_long.csv", row.names = FALSE)  
