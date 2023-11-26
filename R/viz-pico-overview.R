# add table 
# add table 
# add table 
build_pico_overview <- function(
    pico_results,
    table_col_width = unit(2, "cm"),
    omargin = unit(.5, "cm"),
    xlab = "Pairwise Comparisons Extracted [n]",
    ylab = "Farming Practice",
    sizelab = "Unique Metrics Extracted [n]"
) {
  browser()
  
  # Setup ---------------------------------------------------------
  
  line_width <- line_width / 2

  # Count pairwise comparisons ---------------------------------

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
  
  # Count metrics --------------------------------------------------
  
  metrics <- 
    pico_results %>% 
    distinct(fpid, metric_std) %>% 
    count(fpid, sort = T) %>% 
    rename(n_metrics = n)

  # Count experiments ---------------------------------------------
  
  
  
  # Draft main plot -------------------------------------------------
  
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
    geom_vline(xintercept = 0,
               linewidth = line_width) +
    labs(x = xlab,
         y = ylab,
         size = sizelab) +
    scale_radius(range = c(0, 15)) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(c(0, .1)),
      position = "top"
    ) +
    scale_y_discrete(expand = expansion(.03)) +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank())
  
  # extract legend and plot grob ----------------------------------
  
  size_guide <- 
    p %>% 
    get_legend()
  
  p_grob <- 
    p %>% 
    ggplotGrob() %>% 
    gtable_filter(pattern = "panel|axis|xlab-t")
  
  p_grob %>% gtable_show_layout()
  
  # put it all together -------------------------------------------
  
  p_out <- 
    p_grob %>% 
    gtable_add_cols(
      widths = table_col_width,
      pos = 1
    ) %>% 
    gtable_add_cols(
      widths = omargin,
      pos = 0
    ) %>% 
    gtable_add_cols(
      widths = omargin,
      pos = -1
    ) %>% 
    gtable_add_rows(
      heights = omargin,
      pos = 0
    ) %>% 
    gtable_add_rows(
      heights = omargin,
      pos = -1
    )
  
  p_out %>% gtable_show_layout()
  p_out %>% grid.draw()
  
  return(p_out)  
}
