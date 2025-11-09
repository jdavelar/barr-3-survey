#####################################
#### CONVERT ACS TXT FILE TO CSV ####
#####################################

library(here)
library(tidyverse)
library(rio)

# Import all datasets
ct <- import(here("data/ct_languages.txt")) %>% janitor::clean_names()
ma <- import(here("data/ma_languages.txt")) %>% janitor::clean_names()
me <- import(here("data/me_languages.txt")) %>% janitor::clean_names()
nh <- import(here("data/nh_languages.txt")) %>% janitor::clean_names()
ri <- import(here("data/ri_languages.txt")) %>% janitor::clean_names()
vt <- import(here("data/vt_languages.txt")) %>% janitor::clean_names()
dictionary <- import(here("data/acs_languages_dictionary.xlsx")) %>% 
  janitor::clean_names() %>% 
  mutate(field = str_to_lower(field),
         field = str_replace(field, "_(?=(est|moe|pct)$)", ""))

# Function to clean
clean_state_languages <- function(data){
  # Pull relevant cols and labels
  cols <- dictionary %>% 
    filter(!is.na(label)) %>% 
    pull(field)
  labels <- dictionary %>% 
    filter(!is.na(label))
  labels <- setNames(labels$field, labels$label)
  
  # Condense data
  dat <- data %>% 
    select(geography, all_of(cols)) %>% 
    # split geo column
    mutate(state = str_sub(geography, -2),
           district = str_trim(str_remove(geography, ",?\\s*[A-Z]{2}$"))) %>% 
    # recode NAs
    mutate(across(ends_with("pct"), ~na_if(., -888888888))) %>% 
    rename(!!!labels) %>% 
    select(district, state, starts_with("pct"))
  
  return(dat)
}

# Clean datasets
clean_ct <- clean_state_languages(ct)
clean_ma <- clean_state_languages(ma)
clean_me <- clean_state_languages(me)
clean_nh <- clean_state_languages(nh)
clean_ri <- clean_state_languages(ri)
clean_vt <- clean_state_languages(vt)

# Merge
clean <- bind_rows(clean_ct, clean_ma, clean_me, clean_nh, clean_ri, clean_vt)
# Save
write.csv(clean, "data/clean_acs_language.csv", row.names = FALSE)
