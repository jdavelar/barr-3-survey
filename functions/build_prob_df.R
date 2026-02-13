#### BUILD PROBABILITY DATA FRAME ####
# For use with district_non_priority.Rmd

build_prob_df <- function(
    data,
    priority_cols,          # character vector of 0/1 (or TRUE/FALSE) selected indicators
    group_vars,             # character vector of grouping variables
    min_n_group = 10        # flag small group Ns
) {
  library(dplyr)
  library(tidyr)
  
  data <- droplevels(data)
  
  # Make grouping vars a common type so pivot_longer doesn't error
  data2 <- data %>%
    mutate(across(all_of(group_vars), ~ as.character(.x)))
  
  # Long format: one row per respondent × group var × priority
  long <- data2 %>%
    select(all_of(group_vars), all_of(priority_cols)) %>%
    pivot_longer(
      cols = all_of(group_vars),
      names_to = "group_var",
      values_to = "group_level"
    ) %>%
    pivot_longer(
      cols = all_of(priority_cols),
      names_to = "priority",
      values_to = "selected"
    ) %>%
    filter(!is.na(group_level), !is.na(selected)) %>%
    mutate(
      selected = as.integer(selected),
      not_selected = 1L - selected
    )
  
  out <- long %>%
    group_by(group_var, group_level, priority) %>%
    summarise(
      n_group = n(),
      pct_not_selected = mean(not_selected, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      small_n_flag = n_group < min_n_group
    ) %>%
    arrange(group_var, group_level, priority)
  
  return(out)
}
