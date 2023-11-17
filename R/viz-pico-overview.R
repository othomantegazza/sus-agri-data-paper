build_pico_overview <- function(
    pico_results
) {
  pairwise_comparisons <- 
    pico_results %>% 
    distinct(
      fpid,
      intervention_matrix,
      control_matrix
    ) %>% 
    count(
      fpid,
      sort = T
    ) %>% 
    rename(n_pairwise_comparisons = n) 
  
  metrics <- 
    pico_results %>% 
    distinct(fpid, metric_std) %>% 
    count(fpid, sort = T) %>% 
    rename(n_metrics = n)

  p <-
    pairwise_comparisons %>% 
    left_join(metrics) %>% 
    mutate(fpid = fpid %>% as_factor() %>% fct_rev()) %>% 
    ggplot() +
    aes(y = fpid,
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
         y = "Farming Practice",
         size = "Unique Metrics Extracted [n]") +
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
