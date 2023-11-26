# add table 
# add table 
# add table 
build_pico_overview <- function(
    pico_results,
    table_col_width = unit(1, "cm"),
    omargin = unit(.5, "cm"),
    xlab = "Pairwise Comparisons Extracted [n]",
    ylab = "Farming Practice",
    sizelab = "Unique Metrics Extracted [n]"
) {
  # Setup ---------------------------------------------------------
  
  line_width <- line_width / 2
  scale_y_expansion <- 
    scale_y_discrete(expand = expansion(.03))

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
  
  # Put data together ---------------------------------------------
  
  to_main_plot <- 
    pairwise_comparisons %>% 
    left_join(metrics) %>% 
    mutate(fpid = fpid %>% as_factor() %>% fct_rev())

  # Count effects extracted ----------------------------------------
  
  effect_extracted <- 
    pico_results %>%
    count(fpid) %>%
    rename(n_effects = n) %>% 
    mutate(
      fpid = fpid %>% factor(
        levels = levels(to_main_plot$fpid)
      )
    )
  
  # Draft effect Table ---------------------------------------------
  
  p_table <-
    effect_extracted %>% 
    ggplot() +
    aes(x = 1,
        y = fpid) +
    geom_label(
      aes(label = paste0(" ", n_effects)),
      size = text_size_plot,
      label.size = 0,
      fill = "white",
      label.padding = unit(.02, "cm"),
      hjust = 0,
      
    ) +
    geom_vline(xintercept = 1,
               linewidth = line_width) +
    scale_x_continuous(
      limits = c(1, 1.3),
      expand = expansion(0)
    ) +
    scale_y_expansion +
    theme(panel.grid.major.x = element_blank()) 
  
  p_table <- 
    p_table %>% 
    ggplotGrob() %>% 
    gtable_filter("panel")
  
  # Draft main plot ------------------------------------------------
  
  p <-
    to_main_plot %>% 
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
    scale_y_expansion +
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
    ) %>% 
    gtable_add_grob(
      grobs = p_table,
      t = 4, l = 3
    )
  
  p_out %>% gtable_show_layout()
  p_out %>% grid.draw()
  
  return(p_out)  
}
