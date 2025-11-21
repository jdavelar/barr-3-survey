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
messy_sheet <- import(here("data/NH/messy_contact_list.csv"))
messy_crosswalk <- import(here("data/NH/messy_sheet_crosswalk.csv")) #matches our contact - use this
#nh1 <- import(here("data/NH/NH_1.csv"))
nh2 <- import(here("data/NH/NH_2.xlsx"))
nh3 <- import(here("data/NH/NH_3.csv"))
nh4 <- import(here("data/NH/NH_4.csv"))
nh5 <- import(here("data/NH/NH_5.csv"))
nh6 <- import(here("data/NH/NH_6.csv"))
nh13 <- import(here("data/NH/NH_13.csv"))
nh17 <- import(here("data/NH/NH_17.csv"))
# nh18 <- import(here("data/NH/NH_18.csv"))
# nh20 <- import(here("data/NH/NH_20.csv"))

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
  ungroup() %>% 
  #pull unique spans
  select(district_code, district, grades_served) %>% 
  distinct() %>% 
  # filter to districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, grades_served) %>% 
  filter(!is.na(district_code))

rm(grade_crosswalk)

#### DISTRICT TYPE ####
# File name - messy contact sheet
# district_type = Traditional / Regional / Regional CTE / Charter
district_type <- messy_sheet %>% 
  # simple clean
  janitor::clean_names() %>% 
  select(messy_id = sau, messy_name = sau_name, district_type = district_types) %>% 
  mutate(district_type = case_when(
    district_type == "Public Charter District" ~ "Charter",
    district_type == "Traditional District" ~ "Traditional",
    district_type == "Public Academies and Joint Maintenance Agreements" ~ "Regional",
    district_type == "Public Interstate Schools" ~ "Regional"
  )) %>% 
  #filter to relevant cols
  right_join(messy_crosswalk, by = c("messy_id", "messy_name")) %>% 
  select(district_code = messy_id, district = messy_name, district_type) %>% 
  filter(!is.na(district_code))
rm(messy_sheet)

#### URBANICITY ####
# File name = nces_geo_codes & nces_geo_crosswalk & nh_crosswalk_alt
# urbanicity = Urban / Suburban / Rural
nces_crosswalk <- import(here("data/NH/NH_nces_crosswalk.csv"))
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
  filter(!is.na(district_code)) %>% 
  select(district_code, district = nh_matched_district, urbanicity) %>% 
  right_join(nces_crosswalk, by = c("district_code", "district")) %>% 
  select(district, district_code, urbanicity) %>% 
  filter(!is.na(district_code))
rm(nces_geo, nces_geo_codes, nces_geo_crosswalk, nces_crosswalk)

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
  summarize(num_schools = n_distinct(school_name), .groups = "drop") %>% 
  mutate(district_code = as.numeric(district_code)) %>% 
  #filter to districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, num_schools) %>% 
  filter(!is.na(district_code))

#### NUMBER OF HIGH SCHOOLS IN DISTRICT ####
# File = nh6
# defined hs as schools where the max grade is >8
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
  #right_join(nh_crosswalk, by = c("district", "district_code")) %>% 
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, num_high_schools) %>% 
  filter(!is.na(district_code)) %>% 
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
  #right_join(nh_crosswalk, by = c("district", "district_code")) #good
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, enrollment_2025) %>% 
  filter(!is.na(district_code))

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
  #right_join(nh_crosswalk, by = c("district", "district_code")) %>% #9 districts were not in 19-20
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, enrollment_2020) %>% 
  filter(!is.na(district_code)) %>% 
  left_join(enrollment, by = c("district", "district_code")) %>% 
  # create column
  mutate(enrollment_change_6yr_pct = round((enrollment_2025 - enrollment_2020)/enrollment_2020, 2)) %>% 
  select(district, district_code, enrollment_change_6yr_pct)
rm(nh13, nh5)

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
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(-c(district_code, district)) %>% 
  rename(district_code = messy_id,
         district = messy_name) %>% 
  filter(!is.na(district_code))
rm(nh4)

#### PCT LOW INCOME ####
# File nh3
lowinc <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Economically Disadvantaged Students (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_lowinc = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  # impute midpoint (5) for suppression due to <10%
  mutate(pct_lowinc = str_remove_all(pct_lowinc, "%"),
         pct_lowinc = case_when(
           pct_lowinc == "<10.00" ~ 5,
           pct_lowinc == "*N" ~ NA,
           pct_lowinc == "N/A" ~ NA,
           pct_lowinc == "" ~ NA,
           is.na(pct_lowinc) ~ NA,
           TRUE ~ as.numeric(pct_lowinc)
         ),
         pct_lowinc = round(pct_lowinc/100, 2)) %>% 
  #filter to districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, pct_lowinc) %>% 
  filter(!is.na(district_code))

#### PCT ENGLISH LEARNERS ####
# File crdc EL counts
# el <- nh3 %>% 
#   #simple clean
#   janitor::clean_names() %>% 
#   filter(indicator_name == "English Language Learner (%)") %>% 
#   select(district = entity_name, district_code = entity_id, pct_el = x2024) %>% 
#   #clean up pct col - leaving NA for suppressed for now
#   # considering variations of N/A, blanks, and <10% as suppressed
#   # try out different imputation methods
#   mutate(pct_el = str_remove_all(pct_el, "%"),
#          imp_low_pct_el = case_when(
#            pct_el == "<10.00" ~ 1,
#            pct_el == "*N" ~ NA,
#            pct_el == "NA" ~ NA,
#            pct_el == "N/A" ~ NA,
#            is.na(pct_el) ~ NA,
#            TRUE ~ as.numeric(pct_el)
#          ),
#          imp_low_pct_el = round(imp_low_pct_el/100, 2),
#          imp_high_pct_el = case_when(
#            pct_el == "<10.00" ~ 9,
#            pct_el == "*N" ~ NA,
#            pct_el == "NA" ~ NA,
#            pct_el == "N/A" ~ NA,
#            is.na(pct_el) ~ NA,
#            TRUE ~ as.numeric(pct_el)
#          ),
#          imp_high_pct_el = round(imp_high_pct_el/100, 2)) %>% 
#   mutate(district_code = as.numeric(district_code),
#          #pct_el = str_remove_all(pct_el, "%"),
#          pct_el = na_if(pct_el, "<10.00"),
#          pct_el = na_if(pct_el, "*N"),
#          pct_el = na_if(pct_el, ""),
#          pct_el = na_if(pct_el, "N/A"),
#          pct_el = as.numeric(pct_el),
#          pct_el = round(pct_el/100, 2)) %>%   #200/203 missing/suppressed
#   # filter to districts
#   right_join(nh_crosswalk, by = c("district", "district_code"))
# Need to incorporate imputation rule and double check data - almost all missing
# Using alternate method instead below - drawing on CRDC 2021-22 data

test_el <- import(here("data/Across states/2021-22-crdc-data/SCH/Enrollment.csv")) %>% 
  janitor::clean_names() %>% 
  filter(lea_state == "NH" | lea_state == "VT") %>% 
  select(lea_state, leaid, lea_name, schid, sch_name, starts_with("tot_elprogenr"), starts_with("tot_enr")) %>% 
  mutate(across(starts_with("tot"), ~case_when(
    . < 0 ~ NA,
    TRUE ~ .
  ))) %>% 
  group_by(schid, leaid) %>% 
  mutate(tot_elprogenr = sum(c(tot_elprogenr_m, tot_elprogenr_f, tot_elprogenr_x), na.rm = TRUE),
         tot_enr = sum(c(tot_enr_m, tot_enr_f, tot_enr_x), na.rm = TRUE),
         missing = ifelse((is.na(tot_elprogenr_m) & is.na(tot_elprogenr_f) & is.na(tot_elprogenr_x)), 1, 0)) %>% 
  ungroup() %>% 
  group_by(leaid) %>% 
  mutate(district_el_tot = sum(tot_elprogenr),
         district_tot = sum(tot_enr),
         missing = sum(missing)) %>% 
  select(lea_state, district_code = leaid, district = lea_name, n_el = district_el_tot, n_tot = district_tot, missing) %>% 
  distinct() %>% 
  mutate(pct_el = round(n_el/n_tot, 2)) %>% 
  ungroup()
#save
#write.csv(test_el, "data/Across states/crdc_el_counts.csv", row.names = FALSE)
# right join below doesn't work because neither code or district name matches crosswalk or alt crosswalk
# need to do a manual join
crdc_crosswalk <- import(here("data/NH/NH_CRDC_crosswalk.csv"))
el <- test_el %>% 
  filter(lea_state == "NH") %>% 
  select(-n_tot) %>% 
  mutate(district_code = as.numeric(district_code)) %>% 
  rename(crdc_id = district_code, crdc_name = district) %>% 
  right_join(crdc_crosswalk, by = c("crdc_id", "crdc_name")) %>% 
  select(district, district_code, n_el, pct_el) %>% 
  filter(!is.na(district_code))
rm(crdc_crosswalk, test_el)

#### PCT SPECIAL EDUCATION ####
# File nh3
swd <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Students with Disability (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_sped = x2024) %>% 
  #imputing midvalue (5) for suppression due to <10%, NA for N<11
  mutate(pct_sped = str_remove_all(pct_sped, "%"),
         pct_sped = case_when(
           pct_sped == "<10.00" ~ 5,
           pct_sped == "*N" ~ NA,
           pct_sped == "NA" ~ NA,
           pct_sped == "N/A" ~ NA,
           is.na(pct_sped) ~ NA,
           TRUE ~ as.numeric(pct_sped)
         ),
         pct_sped = round(pct_sped/100, 2),
         district_code = as.numeric(district_code)) %>% 
  # filter to districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, pct_sped) %>% 
  filter(!is.na(district_code))

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
  select(district, grade, total = total_fay_students, pct_prof = above_prof_percent_lvl_3_4, subject) %>% 
  # try out different imputations methods
  mutate(pct_prof = case_when(
    pct_prof == "< 10 %" ~ 5,
    pct_prof == "> 90 %" ~ 91,
    pct_prof == "* n < 11" ~ NA,
    TRUE ~ as.numeric(pct_prof)),
    pct_prof = round(pct_prof/100, 2)) %>% 
  separate(total, into = c("min_n", "max_n"), sep = " - ", remove = FALSE) %>% 
  mutate(min_n = as.numeric(min_n),
          max_n = as.numeric(max_n),
          n_prof = round((min_n + max_n) / 2)) %>% 
  select(district, grade, subject, n_prof, pct_prof) %>% 
  pivot_wider(names_from = subject,
              values_from = c(n_prof, pct_prof)) %>% 
  mutate(collapse = ifelse(grade == 11, 0, 1),
         est_prof_math = n_prof_mat * pct_prof_mat,
         est_prof_ela = n_prof_rea * pct_prof_rea) %>% 
  group_by(district, collapse) %>% 
  summarize(n_math_prof = sum(n_prof_mat),
         n_ela_prof = sum(n_prof_rea),
         est_prof_math = sum(est_prof_math),
         est_prof_ela = sum(est_prof_ela),
         pct_math_prof = round(est_prof_math / n_math_prof, 2),
         pct_ela_prof = round(est_prof_ela / n_ela_prof, 2), .groups = "drop") %>% 
    select(district, collapse, n_ela_prof, pct_ela_prof, n_math_prof, pct_math_prof) %>% 
    mutate(collapse = ifelse(collapse == 1, "_3_8", "_9_12")) %>% 
    pivot_longer(cols = c("n_ela_prof", "pct_ela_prof", "n_math_prof", "pct_math_prof"),
                 names_to = "indicator",
                 values_to = "outcome") %>% 
    unite(var_name, indicator, collapse) %>% 
    pivot_wider(names_from = var_name,
                values_from = outcome) %>% 
    #right_join(nh_crosswalk, by = "district")
  right_join(messy_crosswalk, by = c("district")) %>% 
  select(-c(district, district_code)) %>% 
  rename(district = messy_name,
         district_code = messy_id) %>% 
  filter(!is.na(district_code))
rm(nh2)

#### GRADUATION RATE ####
# File = nh3
grad <- nh3 %>% 
  #simple clean
  janitor::clean_names() %>% 
  filter(indicator_name == "Graduation Rate - 4YR (%)") %>% 
  select(district = entity_name, district_code = entity_id, pct_grad = x2024) %>% 
  #clean up pct col - leaving NA for suppressed for now
  mutate(district_code = as.numeric(district_code),
         pct_grad = str_remove_all(pct_grad, "%"),
         pct_grad = na_if(pct_grad, "*N"),
         pct_grad = na_if(pct_grad, ""),
         pct_grad = as.numeric(pct_grad),
         pct_grad = round(pct_grad/100, 2)) %>%  
  # filter to districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, pct_grad) %>% 
  filter(!is.na(district_code))
# No Ns provided

#### CHRONIC ABSENTEEISM ####
# File = nh17
# requested data from state - constructing regular attendance variable in meantime pct_reg_attend
attendance <- nh17 %>% 
  #simple clean
  slice(-c(1:9)) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  janitor::clean_names() %>% 
  slice(-c(1:2, 165:169, 173:175, 208:224)) %>% 
  select(district_code = district_number, district = district_name, pct_reg_attend = total_percent) %>% 
  mutate(district_code = as.numeric(district_code),
         pct_reg_attend = as.numeric(pct_reg_attend),
         pct_reg_attend = round(pct_reg_attend/100, 2)) %>% 
  #filter to relevant districts
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, pct_reg_attend) %>% 
  filter(!is.na(district_code))
rm(nh17)

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
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, per_pupil_spending) %>% 
  filter(!is.na(district_code))
rm(nh3)

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
  #right_join(nh_crosswalk, by = c("district", "district_code"))
  right_join(messy_crosswalk, by = c("district", "district_code")) %>% 
  select(district_code = messy_id, district = messy_name, pct_csi_tsi) %>% 
  filter(!is.na(district_code))
rm(nh6)

#### LANGUAGE USE IN CT ####
# File = acs
# Includes: pct_lang_eo / pct_lang_spanish / pct_lang_aapi / pct_lang_indo / pct_lang_other
language <- acs %>% 
  filter(state == "NH") %>% 
  select(-state) %>% 
  mutate(across(starts_with("pct"), ~round(./100, 2))) %>% 
  # link to districts
  right_join(nh_crosswalk_alt, by = "district") %>% 
  select(district = nh_matched_district, district_code, starts_with("pct")) %>% 
  filter(!is.na(district_code))
rm(acs)
# Failing at join - the district names don't line up; update crosswalk and re-run

#### FTE OF CERTIFIED TEACHERS ####
# File = fte_cert
# Includes: pct_fte_cert (n_fte_cert missing)
fte <- fte_cert %>% 
  filter(lea_state == "NH") %>% 
  select(district = lea_name, pct_fte_cert) %>%
  #filter to districts
  right_join(nh_crosswalk_alt, by = "district") %>%
  select(district = nh_matched_district, district_code, pct_fte_cert) %>% 
  filter(district != "")
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
  full_join(achievement, by = c("district", "district_code")) %>% 
  full_join(grad, by = c("district", "district_code")) %>% 
  full_join(attendance, by = c("district", "district_code")) %>% 
  full_join(spending, by = c("district", "district_code")) %>% 
  full_join(pct_turnaround, by = c("district", "district_code")) %>% 
  full_join(language, by = c("district", "district_code")) %>% 
  full_join(fte, by = c("district", "district_code"))
rm(grades, district_type, urbanicity, num_schools, num_high_schools, enrollment, enrollment_change, race, lowinc, el, swd, achievement, grad, attendance, spending, pct_turnaround, language, fte, nh_crosswalk, nh_crosswalk_alt, messy_crosswalk)

#save
write.csv(clean, "data/clean/clean_nh.csv", row.names = FALSE)

# STILL MISSING:
#  - tenure_years [tbd post-survey]
#  - num_districts [tbd need to pull in full crosswalk to count districts by leader]
#  - attrition_rate or equivalent [reached out to district for help since site download button is malfunctioning]


#### MERGE WITH STATE CROSSWALK FOR SUPT DATA ####
# file = state_crosswalk_long
ne_crosswalk <- import(here("data/Across states/state_crosswalk_long2.csv")) %>% 
  rename(district = district_name) %>% 
  mutate(district_code = as.numeric(district_code),
         district = ifelse(district == "Hanover/SAU #70", "Hanover", district)) #fixing dumb issue where this isn't in long format

final <- ne_crosswalk %>% 
  filter(state == "New Hampshire") %>% 
  right_join(clean, by = c("district", "district_code")) %>% 
  mutate(district = ifelse(district == "Hanover", "Hanover/SAU #70", district)) #HUVESGALUEGVIUSAGVDSAAHIO;

# save
write.csv(final, "data/clean/final_nh.csv", row.names = FALSE)
