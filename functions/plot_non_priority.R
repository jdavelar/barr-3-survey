#### PLOTTING FUNCTIONS FOR LOOKING AT DISTRIBUTION OF SUPTS WHO DID NOT CHOOSE A PRIORITY AREA ####
# This is specifically for the district_non_priority.Rmd file

#### FUNCTION FOR BINARY VARS ####
# Must be in 0/1 format
# n_tab = a df of number of respondents to integrate into title
not_priority_binary <- function(data, var, title, n_tab){
  
  palette <- c("#004B72", "#2A95A0", "#5FBDEB", "#F2AD4B", "#A5CE41")
  
  main_dat <- data %>% 
    select({{ var }}, starts_with("need") & !ends_with("other")) %>% 
    group_by({{ var }}) %>% 
    summarize(across(starts_with("need"), ~sum(., na.rm = TRUE)), .groups = "drop") %>% 
    mutate(across(starts_with("need"), ~ .x / sum(.x)))
  
  plot_dat <- main_dat %>% 
    pivot_longer(
      cols = -{{ var }},
      names_to = "var_name",
      values_to = "pct"
    ) %>% 
    left_join(labs, by = "var_name") %>% 
    left_join(n_tab, by = "var_name") %>% 
    mutate(
      group = ifelse({{ var }} == 1, "Selected", "Did not select"),
      nice_label = paste0(nice_label, " (N = ", n, ")"),
    )
  
  order_vars <- plot_dat %>%
    filter(group == "Selected") %>%
    arrange(desc(pct)) %>%
    distinct(nice_label) %>%
    pull(nice_label)
  
  plot_dat <- plot_dat %>% 
    mutate(nice_label = factor(nice_label, levels = order_vars))
  
  plot <- ggplot(plot_dat, aes(pct, nice_label, fill = group)) +
    geom_col() +
    geom_text(
      aes(label = ifelse(pct > 0, scales::percent(pct, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    scale_fill_manual(values = palette) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = "", y = "", fill = NULL, title = stringr::str_wrap(title, 65)) +
    theme_bw() +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))
  
  plot
}

#### FOR CATEGORICAL VARS ####
not_priority_cat <- function(data, var, title, n_tab,
                             recode_map = NULL,
                             var_levels = NULL) {
  
  palette <- c("#004B72", "#2A95A0", "#5FBDEB", "#F2AD4B", "#A5CE41", "#797A7C")
  
  main_dat <- data %>% 
    select({{ var }}, starts_with("need") & !ends_with("other")) %>% 
    filter(!is.na({{var}})) %>% 
    group_by({{ var }}) %>% 
    summarize(across(starts_with("need"), ~sum(., na.rm = TRUE)), .groups = "drop") %>% 
    mutate(across(starts_with("need"), ~ .x / sum(.x)))
  
  plot_dat <- main_dat %>% 
    pivot_longer(cols = -{{ var }}, names_to = "var_name", values_to = "pct") %>% 
    left_join(labs, by = "var_name") %>%
    left_join(n_tab, by = "var_name") %>% 
    mutate(
      group = as.character({{ var }}),
      nice_label = paste0(nice_label, " (N = ", n, ")"),
    )
  
  if (!is.null(recode_map)) {
    plot_dat <- plot_dat %>% mutate(group = dplyr::recode(group, !!!recode_map))
  }
  
  if (!is.null(var_levels)) {
    plot_dat <- plot_dat %>% mutate(group = factor(group, levels = var_levels))
  }
  
  order_vars <- plot_dat %>%
    filter(group == var_levels[1]) %>%      # anchor ordering to first level (e.g., "10+ years")
    arrange(desc(pct)) %>%
    distinct(nice_label) %>%
    pull(nice_label)
  
  plot_dat <- plot_dat %>%
    mutate(nice_label = factor(nice_label, levels = order_vars))
  
  ggplot(plot_dat, aes(pct, nice_label, fill = group)) +
    geom_col() +
    geom_text(
      aes(label = ifelse(pct > 0, scales::percent(pct, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    scale_fill_manual(values = palette) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = "", y = "", fill = NULL, title = title) +
    theme_bw() +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE))
}