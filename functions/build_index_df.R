#### BUILD INDEX DATAFRAME ####
# For use with district_non_priority.Rmd

library(dplyr)
library(tidyr)
library(purrr)

build_index_df <- function(
    data,
    priority_cols,          # character vector of 0/1 (or TRUE/FALSE) selected indicators
    group_vars,             # character vector of grouping variables (binary or categorical)
    min_n_non = 10          # threshold you may later use for masking
) {
  
  library(dplyr)
  library(tidyr)
  
  data <- droplevels(data)
  
  # IMPORTANT: pivot_longer requires all columns being pivoted to share a type.
  # Make ALL grouping vars character before pivoting.
  data2 <- data %>%
    mutate(across(all_of(group_vars), ~ as.character(.x)))
  
  # Reshape grouping variables long
  g_long <- data2 %>%
    select(all_of(group_vars), all_of(priority_cols)) %>%
    pivot_longer(
      cols = all_of(group_vars),
      names_to = "group_var",
      values_to = "group_level"
    )
  
  # Reshape priorities long
  long <- g_long %>%
    pivot_longer(
      cols = all_of(priority_cols),
      names_to = "priority",
      values_to = "selected"
    ) %>%
    filter(!is.na(group_level), !is.na(selected)) %>%
    mutate(
      selected = as.integer(selected)
    )
  
  # ---- Overall distribution ----
  base <- long %>%
    group_by(group_var, priority, group_level) %>%
    summarise(n_all = n(), .groups = "drop_last") %>%
    mutate(N_all = sum(n_all)) %>%
    ungroup() %>%
    mutate(pct_all = n_all / N_all)
  
  # ---- Non-selector distribution (selected == 0) ----
  non <- long %>%
    filter(selected == 0) %>%
    group_by(group_var, priority, group_level) %>%
    summarise(n_non = n(), .groups = "drop_last") %>%
    mutate(N_non = sum(n_non)) %>%
    ungroup() %>%
    mutate(pct_non = if_else(N_non > 0, n_non / N_non, NA_real_))
  
  # ---- Combine ----
  out <- base %>%
    full_join(non, by = c("group_var", "priority", "group_level")) %>%
    mutate(
      n_all   = coalesce(n_all, 0L),
      N_all   = coalesce(N_all, 0L),
      pct_all = if_else(N_all > 0, n_all / N_all, NA_real_),
      
      n_non   = coalesce(n_non, 0L),
      N_non   = coalesce(N_non, 0L),
      pct_non = if_else(N_non > 0, n_non / N_non, NA_real_),
      
      diff_pp = (pct_non - pct_all) * 100,
      index   = pct_non / pct_all,
      
      small_n_flag = N_non < min_n_non
    ) %>%
    arrange(group_var, group_level, priority)
  
  return(out)
}
