###############################
#### MERGE & CLEAN NH DATA ####
###############################
# load packages
library(here)
library(rio)
library(tidyverse)
library(stringr)

# load data
nh_crosswalk <- import(here("data/NH/NH_crosswalk.csv"))
nh_crosswalk_alt <- import(here("data/NH/NH_crosswalk_alt.csv"))
nces_geo_crosswalk <- import(here("data/Across states/nces_locale_crosswalk.xlsx"))
nces_geo_codes <- import(here("data/Across states/nces_geocodes.xlsx"))
acs <- import(here("data/Across states/clean_acs_language.csv"))
fte_cert <- import(here("data/Across states/crdc_fte_cert.csv"))
nh1 <- import(here("data/NH/NH_1.csv"))
nh2 <- import(here("data/NH/NH_2.xlsx"))
nh3 <- import(here("data/NH/NH_3.csv"))
nh4 <- import(here("data/NH/NH_4.csv"))
nh5 <- import(here("data/NH/NH_5.csv"))
nh6 <- import(here("data/NH/NH_6.csv"))
# nh7 <- import(here("data/NH/NH_7.csv")) #didn't have the info I needed - remove
# nh8 <- import(here("data/NH/NH_8.csv")) #not needed
# nh9 <- import(here("data/NH/NH_9.csv")) #not needed
# nh10 <- import(here("data/NH/NH_10.csv")) #not needed
# nh11 <- import(here("data/NH/NH_11.csv")) #not needed
# nh12 <- import(here("data/NH/NH_12.csv")) #not needed
nh13 <- import(here("data/NH/NH_13.csv"))
# nh14 <- import(here("data/NH/NH_14.csv")) #not needed
# nh15 <- import(here("data/NH/NH_15.csv")) #not needed
# nh16 <- import(here("data/NH/NH_16.csv")) #not needed
nh17 <- import(here("data/NH/NH_17.csv"))
nh18 <- import(here("data/NH/NH_18.csv"))
nh20 <- import(here("data/NH/NH_20.csv"))

#### GRADES SERVED ####
# File name - nh6 & grade_crosswalk
# grades_served = K-6 / K-8 / K-12 / 6-8 / 7-12 / 9-12
grade_crosswalk <- import(here("data/NH/grade_crosswalk.csv"))
grades <- nh6 %>% 
  #simple clean
  slice(2:642) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  select(district_code = dst_id, district = dist_name, school = sch_id, grade_span) %>% 
  # generate grade spans and code categories
  left_join(grade_crosswalk, by = "grade_span") %>% 
  pivot_longer(cols = c(kindergarten:`12`),
               names_to = "grade",
               values_to = "present",
               names_prefix = "grade_") %>% 
  mutate(grade = ifelse(grade == "kindergarten", 0, grade),
         grade = as.numeric(grade)) %>%
  filter(present == 1) %>% 
  group_by(district) %>% 
  mutate(min_grade = min(grade),
         max_grade = max(grade),
         grades_served = case_when(
           min_grade < 5 & max_grade == 6 ~ "K-6",
           min_grade < 5 & max_grade == 8 ~ "K-8",
           min_grade < 5 & max_grade == 12 ~ "K-12",
           min_grade == 6 & max_grade == 8 ~ "6-8",
           min_grade == 7 & max_grade == 12 ~ "7-12",
           min_grade == 9 & max_grade == 12 ~ "9-12",
           TRUE ~ "Unknown"
         )) %>% 
  ungroup() %>% 
  # manual code the "Unknowns that slipped through
  mutate(grades_served = case_when(
    grades_served == "Unknown" & min_grade == 0 & max_grade == 5 ~ "K-6",
    grades_served == "Unknown" & min_grade == 0 & max_grade == 4 ~ "K-6",
    grades_served == "Unknown" & min_grade == 6 & max_grade == 12 ~ "7-12",
    grades_served == "Unknown" & min_grade == 5 & max_grade == 12 ~ "7-12",
    grades_served == "Unknown" & min_grade == 12 & max_grade == 12 ~ "9-12",
    grades_served == "Unknown" & min_grade == 0 & max_grade == 1 ~ "K-6",
    grades_served == "Unknown" & min_grade == 6 & max_grade == 9 ~ "6-8",
    grades_served == "Unknown" & min_grade == 0 & max_grade == 3 ~ "K-6",
    grades_served == "Unknown" & min_grade == 1 & max_grade == 10 ~ "K-12",
    grades_served == "Unknown" & min_grade == 0 & max_grade == 9 ~ "K-8",
    grades_served == "Unknown" & min_grade == 0 & max_grade == 2 ~ "K-6",
    grades_served == "Unknown" & min_grade == 10 & max_grade == 12 ~ "9-12",
    grades_served == "Unknown" & min_grade == 5 & max_grade == 8 ~ "6-8",
    TRUE ~ grades_served),
    district_code = as.numeric(district_code)) %>% 
  #pull unique spans
  select(district_code, district, grades_served) %>% 
  distinct() %>% 
  # filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))

#### DISTRICT TYPE ####
# File name - nh6
# district_type = Traditional / Regional / Regional CTE / Charter
district_type <- nh6 %>% 
  # simple clean
  slice(2:642) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  select(district_code = dst_id, district = dist_name, sch_type_desc) %>% 
  distinct()
# descriptions are at school-level and differ within district - find correct sheet

#### URBANICITY ####
# File name = nces_geo_codes & nces_geo_crosswalk & nh_crosswalk_alt
# urbanicity = Urban / Suburban / Rural
nces_geo <- nces_geo_codes %>% 
  janitor::clean_names() %>% 
  # narrow down to NH
  filter(state == "NH") %>% 
  select(district = name, locale) %>% 
  # convert to numeric for merging
  mutate(locale = as.numeric(locale)) %>% 
  # pull in crosswalk to create col
  left_join(nces_geo_crosswalk, by = "locale") %>% 
  select(district, urbanicity)
#map onto district list
urbanicity <- nh_crosswalk_alt %>% 
  left_join(nces_geo, by = "district") %>% 
  select(district_code, district = nh_matched_district, urbanicity) 
# Names in district list don't match NCES formatting - create additional col or find sheet with names that match fully
rm(nces_geo, nces_geo_codes, nces_geo_crosswalk)

#### NUMBER OF SCHOOLS IN DISTRICT ####
# File = nh6
# num_schools is numeric
num_schools <- nh6 %>% 
  # simple clean
  slice(2:642) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  select(district_code = dst_id, district = dist_name, school_name) %>% 
  distinct() %>% 
  group_by(district, district_code) %>% 
  summarize(num_schools = n_distinct(school_name), .groups = "drop")

#### NUMBER OF HIGH SCHOOLS IN DISTRICT ####
# File = nh6
# defined hs as schools where the max grade is >8
#hs_grades <- c("9", "10", "11", "12")
num_high_schools <- nh6 %>% 
  # simple clean
  slice(2:642) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  select(district_code = dst_id, district = dist_name, school_name, grade_span) %>% 
  mutate(district_code = as.numeric(district_code)) %>% 
  #manually code high schools
  mutate(hs = case_when(
    str_detect(grade_span, "9") ~ 1,
    str_detect(grade_span, "10") ~ 1,
    str_detect(grade_span, "11") ~ 1,
    str_detect(grade_span, "12") ~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(district, district_code, hs) %>% 
  summarize(num_high_schools = n_distinct(school_name), .groups = "drop") %>% 
  filter(hs == 1) %>% 
  select(-hs) %>% 
  #filter to districts and fill NAs
  right_join(nh_crosswalk, by = c("district", "district_code")) %>% 
  mutate(num_high_schools = ifelse(is.na(num_high_schools), 0, num_high_schools))

#### ENROLLMENT IN 2024-25 ####
# File = nh5
# enrollment_2025 is numeric
enrollment <- nh5 %>% 
  # simple clean
  slice(-c(1:11)) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-c(1:2)) %>% 
  slice(-c(164:166, 170:173, 206:209, 211:218)) %>% 
  select(district_code = district_number, district = district_name, enrollment_2025 = total) %>% 
  mutate(district_code = as.numeric(district_code),
         enrollment_2025 = str_remove_all(enrollment_2025, ","),
         enrollment_2025 = as.numeric(enrollment_2025)) %>% 
  # join to check
  right_join(nh_crosswalk, by = c("district", "district_code")) #good
rm(nh5)

#### 6-YEAR ENROLLMENT CHANGE ####
# File = nh5 & nh13 (pulled enrollment instead since already constructed)
# defined as the difference between 2024-25 and 2019-20 enrollment
enrollment_change <- nh13 %>% 
  # simple clean
  slice(-c(1:9)) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-c(1:2)) %>% 
  slice(-c(164:167, 171:173, 201:209)) %>% 
  select(district_code = district_number, district = district_name, enrollment_2020 = total) %>% 
  mutate(district_code = as.numeric(district_code),
         enrollment_2020 = str_remove_all(enrollment_2020, ","),
         enrollment_2020 = as.numeric(enrollment_2020)) %>% 
  # join to add districts and 2025 totals
  right_join(nh_crosswalk, by = c("district", "district_code")) %>% #9 districts were not in 19-20
  left_join(enrollment, by = c("district", "district_code")) %>% 
  # create column
  mutate(enrollment_change_6yr_pct = round((enrollment_2025 - enrollment_2020)/enrollment_2020, 2)) %>% 
  select(district, district_code, enrollment_change_6yr_pct)
rm(nh13)

#### RACIAL DEMOGRAPHICS ####
# File = nh4
# Includes: pct_white / pct_black / pct_hispanic / pct_aapi / pct_aian / pct_multiracial 
race <- nh4 %>% 
  # simple clean
  slice(-c(1:10)) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-c(1:3, 167:170, 174:178, 211:223)) %>% 
  mutate(district_number = as.numeric(district_number)) %>% 
  #create n and pct cols
  select(district = district_name, 
         district_code = district_number, 
         n_aian = american_indian_or_alaskan_native,
         pct_aian = x,
         n_aapi = asian_or_pacific_islander,
         pct_aapi = x_2,
         n_hispanic = hispanic,
         pct_hispanic = x_3,
         n_black = black_non_hispanic,
         pct_black = x_4,
         n_white = white_non_hispanic,
         pct_white = x_5,
         n_multiracial = multi_race,
         pct_multiracial = x_6,
         total) %>% 
  mutate(across(starts_with("pct"), ~str_remove_all(., "%"))) %>% 
  mutate(across(starts_with("n"), ~str_remove_all(., ","))) %>% 
  mutate(across(starts_with("pct"), ~as.numeric(.))) %>% 
  mutate(across(starts_with("n"), ~as.numeric(.))) %>% 
  mutate(across(starts_with("pct"), ~round(./100, 2))) %>% 
  #filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))
rm(nh4)

#### PCT LOW INCOME ####
# File nh3
lowinc <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Economically Disadvantaged Students (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_lowinc = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  # considering variations of N/A, blanks, and <10% as suppressed
  mutate(district_code = as.numeric(district_code),
         pct_lowinc = str_remove_all(pct_lowinc, "%"),
         pct_lowinc = na_if(pct_lowinc, "<10.00%"),
         pct_lowinc = na_if(pct_lowinc, "<10.00"),
         pct_lowinc = na_if(pct_lowinc, "*N"),
         pct_lowinc = na_if(pct_lowinc, ""),
         pct_lowinc = na_if(pct_lowinc, "N/A"),
         pct_lowinc = as.numeric(pct_lowinc),
         pct_lowinc = round(pct_lowinc/100, 2)) %>% 
  #filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))
# look up suppression rules and decide on imputation method

#### PCT ENGLISH LEARNERS ####
# File nh3
el <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "English Language Learner (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_el = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  # considering variations of N/A, blanks, and <10% as suppressed
  mutate(district_code = as.numeric(district_code),
         pct_el = str_remove_all(pct_el, "%"),
         pct_el = na_if(pct_el, "<10.00%"),
         pct_el = na_if(pct_el, "<10.00"),
         pct_el = na_if(pct_el, "*N"),
         pct_el = na_if(pct_el, ""),
         pct_el = na_if(pct_el, "N/A"),
         pct_el = as.numeric(pct_el),
         pct_el = round(pct_el/100, 2)) %>%  #200/203 missing/suppressed
  # filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))
# Need to incorporate imputation rule and double check data - almost all missing

#### PCT SPECIAL EDUCATION ####
# File nh3
swd <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Students with Disability (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_sped = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  # considering variations of N/A, blanks, and <10% as suppressed
  mutate(district_code = as.numeric(district_code),
         pct_sped = str_remove_all(pct_sped, "%"),
         pct_sped = na_if(pct_sped, "<10.00%"),
         pct_sped = na_if(pct_sped, "<10.00"),
         pct_sped = na_if(pct_sped, "*N"),
         pct_sped = na_if(pct_sped, ""),
         pct_sped = na_if(pct_sped, "N/A"),
         pct_sped = as.numeric(pct_sped),
         pct_sped = round(pct_sped/100, 2)) %>%  
  # filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))
# Need to incorporate imputation rule 

#### ACHIEVEMENT DATA ####
# File nh2
# Includes: pct_math_prof_3_8 / pct_math_prof_9_12 / pct_ela_prof_3_8 / pct_ela_prof_9_12
achievement <- nh2 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(level_of_data == "District Level") %>% 
  filter(subgroup == "All students") %>% 
  filter(subject == "mat" | subject == "rea") %>% 
  filter(grade != "All grades") %>% 
  select(district, grade, total = total_fay_students, pct_prof = above_prof_percent_lvl_3_4)
  # will need to get creative with suppression rules:
  # span of total students given rather than exact number (midpoint?)
  # suppressed above 90%, below 10%, and if N < 11;
  # will likely need to choose percent for those suppressed, and calculate a potential percent for below 11
  # also be aware there's no exact Ns, would have to construct with imputation for all districts

#### GRADUATION RATE ####
# File = nh3
grad <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Graduation Rate - 4YR (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_grad = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  # considering variations of N/A, blanks, and <10% as suppressed
  mutate(district_code = as.numeric(district_code),
         pct_grad = str_remove_all(pct_grad, "%"),
         pct_grad = na_if(pct_grad, "*N"),
         pct_grad = na_if(pct_grad, ""),
         pct_grad = as.numeric(pct_grad),
         pct_grad = round(pct_grad/100, 2)) %>%  
  # filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))
# Need to incorporate imputation rule 
# No Ns provided

#### CHRONIC ABSENTEEISM ####
# File = nh17
# TO DO: double check definition - is this regular attendance?

#### PER PUPIL SPENDING ####
# File = nh3
spending <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Average Cost Per Pupil ($)") %>% 
  select(district = entity_name, district_code = entity_id, per_pupil_spending = x2024) %>% 
  #clean up pct col
  mutate(district_code = as.numeric(district_code),
         per_pupil_spending = str_remove_all(per_pupil_spending, "[^0-9\\.]"),
         per_pupil_spending = na_if(per_pupil_spending, ""),
         per_pupil_spending = as.numeric(per_pupil_spending)) %>%  
  # filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))

#### PCT OF TURNAROUND SCHOOLS ####
# File = nh19
# [constructed from pdf]
pct_turnaround <- nh6 %>% 
  # simple clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-c(1, 642)) %>% 
  select(district = dist_name, district_code = dst_id, school = school_name) %>% 
  # pull in csi/tsi status manually from report
  mutate(csi_tsi = case_when(
    school == "Bluff School" ~ 1,
    school == "Compass Classical Academy Charter School (E)" ~ 1,
    school == "Franklin Middle School" ~ 1,
    school == "Paul A. Smith School" ~ 1,
    school == "Edward Fenn School" ~ 1,
    school == "Beech Street School" ~ 1,
    school == "Parker-Varney School" ~ 1,
    school == "Dr. Norman W. Crisp School" ~ 1,
    school == "Richards Elementary School" ~ 1,
    school == "Unity Elementary School" ~ 1,
    school == "Winchester School" ~ 1,
    school == "Ledyard Charter School" ~ 1,
    school == "Kreiva Academy Public Charter School (H)" ~ 1,
    school == "Making Community Connections Charter School - Monadnock" ~ 1,
    school == "Next Charter School" ~ 1,
    school == "Bud Carlson Academy" ~ 1,
    TRUE ~ 0
  ),
  district_code = as.numeric(district_code)) %>% 
  #summarize at district level
  group_by(district, district_code) %>% 
  summarize(n_csi_tsi = sum(csi_tsi),
            n_schools = n_distinct(school),
            pct_csi_tsi = round(n_csi_tsi/n_schools, 2), .groups = "drop") %>% 
  select(district, district_code, pct_csi_tsi) %>% 
  #filter to districts
  right_join(nh_crosswalk, by = c("district", "district_code"))

#### LANGUAGE USE IN CT ####
# File = acs
# Includes: pct_lang_eo / pct_lang_spanish / pct_lang_aapi / pct_lang_indo / pct_lang_other
language <- acs %>% 
  filter(state == "NH") %>% 
  select(-state) %>% 
  mutate(across(starts_with("pct"), ~round(./100, 2))) %>% 
  # link to districts
  right_join(nh_crosswalk_alt, by = "district") %>% 
  select(district, district_code, starts_with("pct"))
rm(acs)
# Failing at join - the district names don't line up; update crosswalk and re-run

#### FTE OF CERTIFIED TEACHERS ####
# File = fte_cert
# Includes: pct_fte_cert (n_fte_cert missing)
fte <- fte_cert %>% 
  filter(lea_state == "NH") %>% 
  select(district = lea_name, pct_fte_cert) %>%
  #filter to districts
  right_join(nh_crosswalk_alt, by = "district") 
rm(fte_cert)

#######################################
#### MERGING ALL THE DATA TOGETHER ####
#######################################
clean <- grades %>% 
  full_join(district_type, by = c("district", "district_code")) %>% 
  full_join(urbanicity, by = c("district", "district_code")) %>% 
  full_join(num_schools, by = c("district", "district_code")) %>% 
  full_join(num_high_schools, by = c("district", "district_code")) %>% 
  full_join(enrollment, by = c("district", "district_code")) %>% 
  full_join(enrollment_change, by = c("district", "district_code")) %>% 
  full_join(race, by = c("district", "district_code")) %>% 
  full_join(lowinc, by = c("district", "district_code")) %>% 
  full_join(el, by = c("district", "district_code")) %>% 
  full_join(swd, by = c("district", "district_code")) %>% 
  full_join(achieve, by = c("district", "district_code")) %>% 
  full_join(grad, by = c("district", "district_code")) %>% 
  full_join(absence, by = c("district", "district_code")) %>% 
  full_join(spending, by = c("district", "district_code")) %>% 
  full_join(pct_turnaround, by = c("district", "district_code")) %>% 
  full_join(language, by = c("district", "district_code")) %>% 
  full_join(fte, by = c("district", "district_code"))
rm(grades, district_type, urbanicity, num_schools, num_high_schools, enrollment, enrollment_change, race, lowinc, el, swd, achieve, grad, absence, spending, pct_turnaround, language, fte)

#save
write.csv(clean, "data/clean/clean_nh.csv", row.names = FALSE)

# STILL MISSING:
#  - tenure_years [tbd post-survey]
#  - num_districts [tbd need to pull in full crosswalk to count districts by leader]
#  - attrition_rate or equivalent [reached out to district for help since site download button is malfunctioning]