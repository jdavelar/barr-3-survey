###############################
#### MERGE & CLEAN CT DATA ####
###############################
# load packages
library(here)
library(rio)
library(tidyverse)
library(stringr)

# load data
nces_geo_crosswalk <- import(here("data/Across states/nces_locale_crosswalk.xlsx"))
nces_geo_codes <- import(here("data/Across states/nces_geocodes.xlsx"))
acs <- import(here("data/Across states/clean_acs_language.csv"))
fte_cert <- import(here("data/Across states/crdc_fte_cert.csv"))
ct1 <- import(here("data/CT/CT_1.csv"))
ct2 <- import(here("data/CT/CT_2.csv"))
ct3 <- import(here("data/CT/CT_3.csv"))
ct4 <- import(here("data/CT/CT_4.csv"))
ct5 <- import(here("data/CT/CT_5.csv"))
ct6 <- import(here("data/CT/CT_6.csv"))
ct7 <- import(here("data/CT/CT_7.csv"))
ct8 <- import(here("data/CT/CT_8.csv"))
ct9 <- import(here("data/CT/CT_9.csv"))
ct10 <- import(here("data/CT/CT_10.csv"))
ct11 <- import(here("data/CT/CT_11.csv"))
ct12 <- import(here("data/CT/CT_12.csv"))
#ct13 <- import(here("data/CT/CT_13.csv")) #used ct19 instead
#ct14 <- import(here("data/CT/CT_14.csv")) #used ct19 instead
#ct15 <- import(here("data/CT/CT_15.csv")) #not used but unsure why
#ct16 <- import(here("data/CT/CT_16.xlsx")) #replace with NCES data instead
#ct17 <- import(here("data/CT/CT_17.xlsx")) #this is a play file I started pulling language into - ignore
ct18 <- import(here("data/CT/CT_18_categories.csv")) #this has multiple sheets and needs to be specified; created _categories
ct19 <- import(here("data/CT/CT_19.xlsx"))
ct20 <- import(here("data/CT/CT_20.xlsx"))
ct21 <- import(here("data/CT/CT_21.csv"))

#### CREATE DISTRICT & CODE CROSSWALK ####
ct_crosswalk <- ct7 %>% 
  # drop empty row and pull in col names
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  # clean up district codes
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  select(district, district_code) %>% 
  filter(!is.na(district_code))

#### GRADES SERVED ####
# File name - CT 7
# grades_served = K-6 / K-8 / K-12 / 6-8 / 7-12 / 9-12
grades <- ct7 %>% 
  # drop empty row and pull in col names
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  # clean up district codes
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  # code suppressed data as NA and convert all to numeric
  filter(district != "Total") %>% 
  mutate(across(3:17, ~ if_else(str_detect(., "[:punct:]"), NA_character_, .))) %>% 
  mutate(across(3:17, as.numeric)) %>% 
  mutate(across(3:17, ~ifelse(. > 0, 1, 0))) %>% 
  select(-c(pre_kindergarten, total)) %>% 
  # generate grade spans and code categories
  pivot_longer(cols = c(kindergarten, starts_with("grade")),
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
  select(district, district_code, grades_served) %>% 
  unique() %>% 
  # catch outliers
  mutate(grades_served = case_when(
    district == "Cornwall School District" ~ "K-8",
    district == "Jumoke Academy District" ~ "K-12",
    district == "Unified School District #2" ~ "7-12",
    district == "Great Oaks Charter School District" ~ "7-12",
    TRUE ~ grades_served
  ))
# 4 outliers with these grade spans that need to be verified:
# Cornwall School District 0-7; coded K-8
# Jumoke Academy District 0-10; coded K-12
# Unified School District #2 8-12; coded 7-12
# Great Oaks Charter School District 6-12; coded 7-12
# Ends up with: 
# 7-12 9-12 K-12  K-6  K-8 
# 10   10  124   18   37 
rm(ct7)

#### DISTRICT TYPE ####
# File name = CT_13 & CT_14
# district_type = Traditional / Regional / Regional CTE / Charter
district_type <- ct19 %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace(organization_code, "^0+", ""))) %>% 
  right_join(ct_crosswalk, by = c("district", "district_code")) %>% 
  mutate(district_type = case_when(
    org_type == "Public Charter School Districts" ~ "Charter",
    org_type == "Regional School Districts" | org_type == "Regional Education Service Center School Districts" ~ "Regional",
    org_type == "CT Technical Education and Career Districts" ~ "Regional CTE",
    org_type == "State Agencies" ~ "Stage agency",
    TRUE ~ "Traditional"
  )) %>% 
  select(district, district_code, district_type)

#### URBANICITY ####
# File name = nces_geo_codes & nces_geo_crosswalk
# urbanicity = Urban / Suburban / Rural
nces_geo <- nces_geo_codes %>% 
  janitor::clean_names() %>% 
  # narrow down to CT
  filter(state == "CT") %>% 
  select(district = name, locale) %>% 
  # convert to numeric for merging
  mutate(locale = as.numeric(locale)) %>% 
  # pull in crosswalk to create col
  left_join(nces_geo_crosswalk, by = "locale") %>% 
  select(district, urbanicity)
#map onto district list
urbanicity <- ct_crosswalk %>% 
  left_join(nces_geo, by = "district") 
rm(nces_geo, nces_geo_codes, nces_geo_crosswalk)

#### NUMBER OF SCHOOLS IN DISTRICT ####
# File = ct9
# num_schools is numeric
num_schools <- ct9 %>% 
  # basic cleaning
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         district = na_if(district, "")) %>% 
  fill(district, .direction = "down") %>% 
  fill(district_code, .direction = "down") %>% 
  # create col
  group_by(district) %>% 
  mutate(num_schools = n_distinct(school)) %>% 
  select(district, district_code, num_schools) %>% 
  unique()

#### NUMBER OF HIGH SCHOOLS IN DISTRICT ####
# File = ct9 (reference to create crosswalk) & ct19
# defined hs as schools where the max grade is >8
school_crosswalk <- ct9 %>% 
  # basic cleaning
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         district = na_if(district, ""),
         school_code = as.numeric(str_replace_all(school_code, "[^0-9\\.]", ""))) %>% 
  fill(district, .direction = "down") %>% 
  fill(district_code, .direction = "down") %>% 
  select(-count)
num_high_schools <- ct19 %>% 
  #basic clean
  janitor::clean_names() %>% 
  filter(!is.na(district)) %>% 
  #create col
  select(school_program_name, district_code = organization_code, low_grade, high_grade) %>% 
  mutate(high_grade = str_remove_all(high_grade, "g"),
         high_grade = case_when(
           high_grade == "PK" ~ -1,
           high_grade == "K" ~ 0,
           is.na(high_grade) ~ NA,
           TRUE ~ as.numeric(high_grade)
         ),
         hs = ifelse(high_grade > 8, 1, 0),
         school_code = as.numeric(district_code)) %>% 
  select(-district_code) %>% 
  #merge with crosswalk
  right_join(school_crosswalk, by = "school_code") %>% 
  group_by(district) %>% 
  mutate(num_high_schools = sum(hs, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(district, district_code, num_high_schools) %>% 
  unique()
rm(ct9, ct19)

#### ENROLLMENT IN 2024-25 ####
# File = ct1
# enrollment is numeric at district-level
enrollment <- ct1 %>% 
  #basic clean
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  rename(enrollment_2025 = count) %>% 
  #drop district that is not in crosswalk and state total
  filter(district != "Total" & district != "Department of Mental Health and Addiction Services") %>% 
  #figure out Union SD - coded as "*"
  # checked enrollment dashboard online - N = 49
  mutate(enrollment_2025 = ifelse(district == "Union School District", 49, as.numeric(enrollment_2025)))
rm(ct1)

#### 6-YEAR ENROLLMENT CHANGE ####
# File = ct8
# defined as the difference between 2024-25 and 2019-20 enrollment
enrollment_change <- ct8 %>% 
  #basic clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  filter(!is.na(district_code)) %>% 
  mutate(x2024_25 = ifelse(district == "Union School District", 49, as.numeric(x2024_25)),
         x2019_20 = as.numeric(x2019_20),
         enrollment_change_6yr_pct = round((x2024_25 - x2019_20)/x2019_20, 2)) %>% 
  select(district, district_code, enrollment_change_6yr_pct) %>% 
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct8)

#### RACIAL DEMOGRAPHICS ####
# File = ct3
# Includes: pct_white / pct_black / pct_hispanic / pct_aapi / pct_aian / pct_multiracial 
race <- ct3 %>% 
  #basic clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  #rename cols to match
  rename(n_white = white, n_black = black_or_african_american, n_hispanic = hispanic_latino_of_any_race, n_asian = asian, n_aian = american_indian_or_alaska_native, n_multiracial = two_or_more_races, n_nhpi = native_hawaiian_or_other_pacific_islander) %>% 
  # fill suppressed data by defining residual and splitting evenly across groups
  mutate(across(starts_with("n"), ~ na_if(., "*"))) %>% 
  mutate(across(c(3:10), ~ as.numeric(.))) %>% 
  group_by(district, district_code) %>% 
  mutate(residual = total - sum(n_white, n_black, n_hispanic, n_asian, n_nhpi, n_aian, n_multiracial, na.rm = TRUE),
         count_suppressed = sum(is.na(across(c(n_white, n_black, n_hispanic, n_asian, n_nhpi, n_aian, n_multiracial)))),
         imputed_value = residual/count_suppressed) %>% 
  mutate(across(starts_with("n"), ~if_else(is.na(.), imputed_value, .))) %>% 
  ungroup() %>% 
  #create cols
  mutate(pct_aian = n_aian/total,
         pct_aapi = (n_asian + n_nhpi)/total,
         pct_black = n_black/total,
         pct_hispanic = n_hispanic/total,
         pct_multiracial = n_multiracial/total,
         pct_white = n_white/total) %>% 
  mutate(across(starts_with("pct"), ~round(., 2))) %>% 
  mutate(across(starts_with("n"), ~round(.))) %>% 
  select(district, district_code, starts_with("n"), starts_with("pct")) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct3)

#### PCT LOW INCOME ####
# File ct4
lowinc <- ct4 %>% 
  #basic clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  mutate(across(c(3:6), ~ na_if(., "*"))) %>% 
  mutate(across(c(3:6), ~ as.numeric(.))) %>% 
  # fill suppressed data with midpoint (3)
  mutate(across(c(3:6), ~ ifelse(is.na(.), 3, .))) %>% 
  #create cols
  mutate(lowinc = free + reduced,
         pct_lowinc = round(lowinc/total, 2)) %>% 
  select(district, district_code, pct_lowinc) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct4)

#### PCT ENGLISH LEARNERS ####
# File ct5
el <- ct5 %>% 
  #basic clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  mutate(across(c(3:5), ~ na_if(., "*"))) %>% 
  mutate(across(c(3:5), ~ as.numeric(.))) %>% 
  # fill suppressed data with midpoint (3)
  mutate(across(c(3:5), ~ ifelse(is.na(.), 3, .))) %>% 
  #create cols
  mutate(pct_el = round(english_learner_multilingual_learner/total, 2)) %>% 
  select(district, district_code, pct_el) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct5)

#### PCT SPECIAL EDUCATION ####
# File ct5
swd <- ct6 %>% 
  #basic clean
  slice(-1) %>% 
  { setNames(., unlist(.[1, ])) } %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  mutate(across(c(3:5), ~ na_if(., "*"))) %>% 
  mutate(across(c(3:5), ~ as.numeric(.))) %>% 
  # fill suppressed data with midpoint (3)
  mutate(across(c(3:5), ~ ifelse(is.na(.), 3, .))) %>% 
  #create cols
  mutate(pct_sped = round(students_with_disabilities/total, 2)) %>% 
  select(district, district_code, pct_sped) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct6)

#### ACHIEVEMENT DATA ####
# File ct11 & ct21
# Includes: pct_math_prof_3_8 / pct_math_prof_9_12 / pct_ela_prof_3_8 / pct_ela_prof_9_12
achieve_elem <- ct11 %>% 
  #manual clean - subheaders are creating issues with dropping row and pulling in colnames
  select(-c(V5, V7, V8, V10, V12, V14, V16, V19)) %>% 
  rename(district = V1, 
         district_code = V2, 
         grade = V3, 
         subject = V4, 
         total_tested = V6, 
         level_1_n = V9,
         level_2_n = V11,
         level_3_n = V13,
         level_4_n = V15,
         n_prof = V17, 
         pct_prof = V18) %>% 
  slice(-c(1, 2)) %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         district = na_if(district, ""),
         grade = na_if(grade, ""),
         grade = as.numeric(grade)) %>% 
  mutate(across(starts_with("level"), ~na_if(., "*"))) %>% 
  mutate(across(ends_with("prof"), ~na_if(., "*"))) %>% 
  fill(district, .direction = "down") %>% 
  fill(district_code, .direction = "down") %>% 
  fill(grade, .direction = "down") %>% 
  mutate(across(5:11, ~as.numeric(.))) %>% 
  # fill suppressed data by defining residual and splitting evenly across groups
  group_by(district, district_code, grade, subject) %>% 
  mutate(residual = total_tested - sum(level_1_n, level_2_n, level_3_n, level_4_n, na.rm = TRUE),
         count_suppressed = sum(is.na(across(c(level_1_n, level_2_n, level_3_n, level_4_n)))),
         imputed_value = residual/count_suppressed) %>% 
  mutate(across(starts_with("level"), ~if_else(is.na(.), imputed_value, .))) %>% 
  mutate(n_prof = ifelse(is.na(n_prof), sum(level_3_n, level_4_n, na.rm = TRUE), n_prof),
         pct_prof = ifelse(is.na(pct_prof), round(n_prof/total_tested), pct_prof),
         n_prof = round(n_prof)) %>% 
  ungroup() %>% 
  # create cols aggregating across grades 3-8
  group_by(district, district_code, subject) %>% 
  summarize(n_prof = sum(n_prof, na.rm = TRUE),
            total = sum(total_tested, na.rm = TRUE),
            pct_prof = round(n_prof/total, 2), .groups = "drop") %>% 
  select(-total) %>% 
  pivot_wider(names_from = subject,
              values_from = c(n_prof, pct_prof)) %>% 
  rename(n_math_prof_3_8 = n_prof_Math,
         pct_math_prof_3_8 = pct_prof_Math,
         n_ela_prof_3_8 = n_prof_ELA,
         pct_ela_prof_3_8 = pct_prof_ELA) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))

achieve_high <- ct21 %>% 
  #manual clean - subheaders are creating issues with dropping row and pulling in colnames
  select(-c(V4, V6, V7, V9, V11, V13, V15, V18)) %>% 
  rename(district = V1, 
         district_code = V2, 
         subject = V3, 
         total_tested = V5, 
         level_1_n = V8,
         level_2_n = V10,
         level_3_n = V12,
         level_4_n = V14,
         n_prof = V16, 
         pct_prof = V17) %>% 
  slice(-c(1, 2)) %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         district = na_if(district, "")) %>% 
  mutate(across(starts_with("level"), ~na_if(., "*"))) %>% 
  mutate(across(ends_with("prof"), ~na_if(., "*"))) %>% 
  fill(district, .direction = "down") %>% 
  fill(district_code, .direction = "down") %>%  
  mutate(across(4:10, ~as.numeric(.))) %>% 
  # fill suppressed data by defining residual and splitting evenly across groups
  group_by(district, district_code, subject) %>% 
  mutate(residual = total_tested - sum(level_1_n, level_2_n, level_3_n, level_4_n, na.rm = TRUE),
         count_suppressed = sum(is.na(across(c(level_1_n, level_2_n, level_3_n, level_4_n)))),
         imputed_value = residual/count_suppressed) %>% 
  mutate(across(starts_with("level"), ~if_else(is.na(.), imputed_value, .))) %>% 
  mutate(n_prof = ifelse(is.na(n_prof), sum(level_3_n, level_4_n, na.rm = TRUE), n_prof),
         pct_prof = ifelse(is.na(pct_prof), round(100*(n_prof/total_tested), 1), pct_prof),
         pct_prof = round(pct_prof/100, 2),
         n_prof = round(n_prof)) %>% 
  ungroup() %>% 
  # create cols aggregating across grades 3-8
  select(district, district_code, subject, n_prof, pct_prof) %>% 
  pivot_wider(names_from = subject,
              values_from = c(n_prof, pct_prof)) %>% 
  rename(n_math_prof_9_12 = n_prof_Math,
         pct_math_prof_9_12 = pct_prof_Math,
         n_ela_prof_9_12 = n_prof_ELA,
         pct_ela_prof_9_12 = pct_prof_ELA) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))

# Combine elem/high
achieve <- achieve_elem %>% 
  left_join(achieve_high, by = c("district", "district_code")) %>% 
  mutate(across(starts_with("pct"), ~ na_if(., NaN))) %>% 
  select(district, district_code, n_ela_prof_3_8, pct_ela_prof_3_8, n_math_prof_3_8, pct_math_prof_3_8, n_ela_prof_9_12, pct_ela_prof_9_12, n_math_prof_9_12, pct_math_prof_9_12)
rm(ct11, ct21, achieve_elem, achieve_high)

#### GRADUATION RATE ####
# File = ct10
grad <- ct10 %>% 
  #manual clean because of value in row 1
  select(district = V1, district_code = V2, grad_rate = V5) %>% 
  slice(-c(1, 2)) %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         grad_rate = na_if(grad_rate, "*"), #suppressed data
         grad_rate = as.numeric(grad_rate),
         grad_rate = round(grad_rate/100, 2)) %>% 
  #fill suppressed data manually (looked up non-graduate data and subtracted from total, calculated pct)
  mutate(grad_rate = case_when(
    district_code == 3360015 ~ .04, #unified SD1 non-grad = 44/46
    district_code == 2110012 ~ 1, #regional school district 11 = 0/11; confirmed grad count is 11 and unsure why it created NA
    district_code == 2860013 ~ .82, #highville charter SD grad = 14/17; confirmed and unsure why it created NA
    TRUE ~ grad_rate
  )) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
# Note there are still missing NAs introduced after joining. A quick check showed missing data or missing district in CT portal;
# could be new districts that don't have 23-24 data or some other weird quirk.
rm(ct10)

#### CHRONIC ABSENTEEISM ####
# File = ct2
absence <- ct2 %>% 
  #manual clean because of value in row 1
  select(district = V1, district_code = V2, pct_chronic_absenteeism = V4) %>% 
  slice(-c(1, 2)) %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", "")),
         pct_chronic_absenteeism = na_if(pct_chronic_absenteeism, "*"),
         pct_chronic_absenteeism = as.numeric(pct_chronic_absenteeism),
         pct_chronic_absenteeism = round(pct_chronic_absenteeism/100, 2)) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code")) %>% 
  #manually impute missing rates; used month by district data in portal
  mutate(pct_chronic_absenteeism = case_when(
    district_code == 310011 ~ .09, #Cornwall SD = Oct '24 - Feb '25
    district_code == 530011 ~ .05, #Franklin SD
    district_code == 650011 ~ .06, #Hartland SD
    district_code == 980011 ~ .09, #Norfolk SD - slightly different approach; only 2 values of 12.5 with others suppressed. Highest possible value would've been .089 with suppression rules so I adjusted down to 9%
    district_code == 1450011 ~ .2, #Union SD
    TRUE ~ pct_chronic_absenteeism
  ))
rm(ct2)

#### PER PUPIL SPENDING ####
# File = ct12
spending <- ct12 %>% 
  #basic clean
  janitor::clean_names() %>% 
  mutate(district_code = as.numeric(str_replace_all(district_code, "[^0-9\\.]", ""))) %>% 
  filter(`function` == "Total") %>% 
  #create col
  mutate(per_pupil_spending = str_remove_all(ppe, "[^0-9\\.]"),
         per_pupil_spending = as.numeric(per_pupil_spending)) %>% 
  select(district, district_code, per_pupil_spending) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct12)

#### PCT OF TURNAROUND SCHOOLS ####
# File = ct18
pct_turnaround <- ct18 %>% 
  #basic clean
  janitor::clean_names() %>% 
  select(district = district_name, district_code, school = school_name, support_type) %>% 
  #create col
  group_by(district, district_code) %>% 
  mutate(support_type = na_if(support_type, ""),
         csi_tsi = ifelse(!is.na(support_type), 1, 0)) %>% 
  summarize(n_csi_tsi = sum(csi_tsi),
            total = n_distinct(school),
            pct_csi_tsi = round(n_csi_tsi/total, 2), .groups = "drop") %>% 
  select(district, district_code, pct_csi_tsi) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = c("district", "district_code"))
rm(ct18)

#### LANGUAGE USE IN CT ####
# File = acs
# Includes: pct_lang_eo / pct_lang_spanish / pct_lang_aapi / pct_lang_indo / pct_lang_other
language <- acs %>% 
  filter(state == "CT") %>% 
  select(-state) %>% 
  mutate(across(starts_with("pct"), ~round(./100, 2))) %>% 
  # link to districts
  right_join(ct_crosswalk, by = "district") %>% 
  select(district, district_code, starts_with("pct"))
rm(acs)
rm(school_crosswalk)

#### FTE OF CERTIFIED TEACHERS ####
# File = fte_cert
# Includes: pct_fte_cert (n_fte_cert missing)
fte <- fte_cert %>% 
  filter(lea_state == "CT") %>% 
  select(district = lea_name, pct_fte_cert) %>%
  mutate(pct_fte_cert = round(pct_fte_cert, 2)) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = "district")
rm(fte_cert)

#### STUDENT-TEACHER RATIOS ####
# File = ct20
# Includes: attrition_rate / attrition_n
attrition <- ct20 %>%
  janitor::clean_names() %>% 
  select(district, turnover_description, school_program, count, rate) %>% 
  filter(school_program == "District Total") %>% 
  filter(turnover_description == "Stayed in the district, stayed in the classroom") %>% 
  select(district, retention_n = count, retention_rate = rate) %>% 
  mutate(retention_rate = round(retention_rate, 2)) %>% 
  #filter to districts
  right_join(ct_crosswalk, by = "district")
rm(ct20)


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
  full_join(fte, by = c("district", "district_code")) %>% 
  full_join(attrition, by = c("district", "district_code"))
rm(grades, district_type, urbanicity, num_schools, num_high_schools, enrollment, enrollment_change, race, lowinc, el, swd, achieve, grad, absence, spending, pct_turnaround, language, fte, attrition)

#save
write.csv(clean, "data/clean/clean_ct.csv", row.names = FALSE)

# STILL MISSING:
#  - tenure_years [tbd post-survey]
#  - num_districts [tbd need to pull in full crosswalk to count districts by leader]


#### MERGE WITH STATE CROSSWALK FOR SUPT DATA ####
# file = state_crosswalk_long
ne_crosswalk <- import(here("data/Across states/state_crosswalk_long2.csv")) %>% 
  rename(district = district_name) %>% 
  mutate(district_code = as.numeric(district_code))

final <- ne_crosswalk %>% 
  filter(state == "Connecticut") %>% 
  right_join(clean, by = c("district", "district_code")) %>% 
  filter(!is.na(last_name)) #dropping the 3 districts that were in the CT data but not the New England crosswalk
# these are private schools serving as HS in some towns

# save
write.csv(final, "data/clean/final_ct.csv", row.names = FALSE)
