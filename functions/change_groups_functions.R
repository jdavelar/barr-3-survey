##################################################
#### FUNCTIONS FOR CHANGE GROUPS DESCRIPTIVES ####
##################################################

cat_table <- function(data, group = change_group, var, caption) {
  #generate new data with counts/pct
  new_dat <- data %>% 
    group_by({{group}}) %>% 
    count({{var}}) %>% 
    mutate(pct = round(n/sum(n), 2)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = {{group}},
                values_from = c(n, pct)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    select({{var}}, `n_0 domains`, `pct_0 domains`, `n_1-2 domains`, `pct_1-2 domains`, `n_3-5 domains`, `pct_3-5 domains`, `n_6-9 domains`, `pct_6-9 domains`)
  
  #set up table structure
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 1, ""),
        th(colspan = 2, '0 domains'),
        th(colspan = 2, '1-2 domains'),
        th(colspan = 2, '3-5 domains'),
        th(colspan = 2, "6-9 domains")
      ),
      tr(
        lapply(c("Group", rep(c('N', '%'), 4)), th)
      )
    )
  ))
  
  #display table
  table <- datatable(new_dat,
            caption = caption,
            colnames = c("", rep(c("N", "%"), 4)),
            container = sketch,
            rownames = FALSE)
  
  print(table)
  
  return(new_dat)
}

cat_stacked_bar <- function(data, var, title, subtitle = NULL, x_lab, y_lab) {
  data %>% 
    select({{var}}, starts_with("n")) %>% 
    pivot_longer(cols = starts_with("n"),
                 names_to = "change_group",
                 values_to = "n",
                 names_prefix = "n_") %>% 
    group_by({{var}}) %>% 
    mutate(pct = n/sum(n)) %>% 
    ungroup() %>% 
    ggplot(., aes(pct, {{var}}, fill = change_group)) +
    geom_col(position = position_stack(reverse = TRUE), aes(group = change_group)) +
    theme_bw() +
    labs(title = title,
         x = x_lab,
         y = y_lab,
         fill = "Change group") +
    scale_fill_manual(values = c("#f0b8b8", "#e67f83", "#aecdc2", "#1F916F")) +
    geom_text(aes(x = ave(pct, {{var}}, FUN = cumsum) - pct/2,
                  label = ifelse(!is.na(pct) & pct > 0, scales::percent(pct, accuracy = 1), "")),
              position = "identity",
              size = 3.5,
              show.legend = FALSE) 
}

cont_table <- function(data, group = change_group, var, round_vars, caption) {
  #generate descriptive stats
  new_dat <- data %>% 
    group_by({{group}}) %>% 
    summarize(mean = mean({{var}}, na.rm = TRUE),
              sd = sd({{var}}, na.rm = TRUE),
              min = min({{var}}, na.rm = TRUE),
              max = max({{var}}, na.rm = TRUE)) %>% 
    mutate(across({{round_vars}}, ~round(., 2)))
  
  #display table
  table <- datatable(new_dat, 
                     caption = caption)
  
  print(table)
  
  return(new_dat)
}

cont_boxplot <- function(data, group = change_group, var, title, x_lab, y_lab){
  viz <- ggplot(data, aes({{group}}, {{var}})) +
    geom_boxplot() +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(title = title,
         subtitle = str_wrap("These boxplots represent the median (bold line), 25th percentile (bottom of box), 75th percentile (top of box), and outliers (dots).", 100),
         x = x_lab,
         y = y_lab)
  
  print(viz)
}
