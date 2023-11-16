build_pico_impact_overview <- function(
    pico_results
) {
  browser()
  pairwise_comparisons <- 
    pico_results %>% 
    count(impact_matrix, sort = T) %>% 
    rename(n_pairwise_comparisons = n) 
  
  metrics <- 
    pico_results %>% 
    distinct(impact_matrix, metric_std) %>% 
    count(impact_matrix, sort = T) %>% 
    rename(n_metrics = n)
  
  
  p <-
    pairwise_comparisons %>% 
    left_join(metrics) %>% 
    mutate(
      impact_matrix = impact_matrix %>% as_factor() %>% fct_rev()
    ) %>% 
    ggplot() +
    aes(y = impact_matrix,
        x = n_pairwise_comparisons) +
    geom_point(
      aes(size = n_metrics),
      shape = 21,
      fill = "#FFFFFF00"
    ) +
    geom_point(
      size = .5,
      shape = 3,
      fill = "#FFFFFF00"
    ) +
    labs(x = "Pairwise Comparisons Extracted [n]",
         y = "Impact Measured",
         size = "Unique Classes of Metrics Extracted [n]") +
    scale_radius(range = c(0, 15)) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(c(0, .1))
    ) +
    scale_y_discrete(expand = expansion(.03)) +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_line(
            linewidth = line_width/2
          ))
  
  return(p)  
}
