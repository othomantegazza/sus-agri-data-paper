build_pico_overview <- function(
    pico_results,
    id_col = fpid,
    table_col_width = unit(2, "cm"),
    omargin = unit(.5, "cm"),
    labels_height = unit(2, "cm"),
    spacer_width = unit(.1, "cm"),
    xlab = "Unique intervention-comparator pairs extracted",
    ylab = "Farming Practice extracted",
    table_lab = "Results of statistical tests extracted",
    sizelab = "Unique outcome metrics extracted:",
    circle_alpha = .5
) {
  # browser()
  # Setup ---------------------------------------------------------
  
  id_col <- enquo(id_col)
  
  line_width <- line_width / 2
  scale_y_expansion <- 
    scale_y_discrete(expand = expansion(.03))
  gp <- gpar(
    fontsize = base_size,
    fontface = "italic"
  )
  table_x_low <- 1
  table_x_high <- 1.3
  
  # Count pairwise comparisons ---------------------------------

  pairwise_comparisons <- 
    pico_results %>% 
    distinct(
      !!id_col,
      intervention_matrix,
      control_matrix
    ) %>% 
    count(
      !!id_col,
      sort = T
    ) %>% 
    rename(n_pairwise_comparisons = n) 
  
  # Count metrics --------------------------------------------------
  
  metrics <- 
    pico_results %>% 
    distinct(!!id_col, metric_std) %>% 
    count(!!id_col, sort = T) %>% 
    rename(n_metrics = n)
  
  # Put data together ---------------------------------------------
  
  to_main_plot <- 
    pairwise_comparisons %>% 
    left_join(metrics) %>% 
    mutate({{id_col}} := !!id_col %>% as_factor() %>% fct_rev())

  # Count effects extracted ----------------------------------------
  
  effect_extracted <- 
    pico_results %>%
    count(!!id_col) %>%
    rename(n_effects = n) %>% 
    mutate(
      {{id_col}} := !!id_col %>% factor(
        levels = levels(to_main_plot %>% pull(!!id_col))
      )
    )
  
  # Draft effect Table ---------------------------------------------
  
  p_table <-
    effect_extracted %>% 
    ggplot() +
    aes(x = 1,
        y = !!id_col) +
    geom_label(
      aes(label = paste0(" ", n_effects)),
      size = text_size_plot,
      label.size = 0,
      fill = "white",
      label.padding = unit(.02, "cm"),
      hjust = 0,
      
    ) +
    geom_vline(xintercept = table_x_low,
               linewidth = line_width) +
    geom_vline(xintercept = table_x_high,
               linewidth = line_width) +
    scale_x_continuous(
      limits = c(table_x_low, table_x_high),
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
    aes(y = !!id_col,
        x = n_pairwise_comparisons) +
    geom_point(
      aes(size = n_metrics),
      shape = 21,
      # fill = "#FFFFFF00",
      fill = alpha(fill_color, circle_alpha)
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
    scale_radius(range = c(0, 15),
                 guide = guide_legend(
                   direction = "horizontal",
                   title.position = "top"
                 )) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(c(0, .1)),
      position = "top"
    ) +
    scale_y_expansion +
    theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x.top = element_text(
        face = "italic",
        hjust = 1
      ),
      # axis.ticks.x.top = element_line(
      #   linewidth = line_width/2
      # ),
      axis.ticks.x.top = element_blank(),
      # axis.text.x.top = element_text(hjust = 0),
      legend.background = element_rect(
        colour = "white",
        fill = "white"
      ),
      legend.text = element_text(face = "italic"),
      legend.title = element_text(face = "italic")
    )
  
  # Extract legend ------------------------------------------------
  
  # black_square <- rectGrob(
  #   gp = gpar(fill = "black")
  # )
  
  size_guide <- 
    p %>% 
    get_legend() %>% 
    gtable_filter("guides") %>% 
    .$grob %>% 
    .[[1]]
    
  
  
  size_table <- 
    gtable() %>% 
    gtable_add_cols(
      widths = unit(1, "null"),
      pos = 0
    ) %>% 
    gtable_add_rows(
      heights = unit(1, "null"),
      pos = 0
    ) %>% 
    gtable_add_cols(
      widths = gtable_width(size_guide),
      pos = 1
    ) %>% 
    gtable_add_rows(
      heights = gtable_height(size_guide),
      pos = 1
    ) %>% 
    # gtable_add_grob(
    #   grobs = rectGrob(
    #     gp = gpar(fill = "black")
    #   ),
    #   t = 1,
    #   l = 1
    # ) %>% 
    gtable_add_grob(
      grobs = size_guide,
      t = 2,
      l = 2
    ) 

  # size_table %>% gtable_show_layout()
  # size_table %>% grid.draw()
  
  # extract plot grob ---------------------------------------------

  p_grob <- 
    p %>% 
    ggplotGrob() %>% 
    gtable_filter(pattern = "panel|axis")
  
  # text labels ---------------------------------------------------

  make_text_label <- function(text) {
    textbox_grob(
      text = text,
      x = unit(0, "npc"),
      y = unit(0, "npc"),
      width = unit(1, "npc"),
      hjust = 0,
      halign = .5,
      vjust = 0,
      gp = gp
    )
  }
  
  label_table <- make_text_label(table_lab)
  xlab_grob <- make_text_label(xlab)
  ylab_grob <- make_text_label(ylab)
  
  # put it all together -------------------------------------------
  
  p_out <- 
    p_grob %>% 
    # table
    gtable_add_cols(
      widths = table_col_width,
      pos = 1
    ) %>% 
    # labels
    gtable_add_rows(
      heights = labels_height,
      pos = 0
    ) %>% 
    # padding
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
    # spacers
    gtable_add_cols(
      widths = spacer_width,
      pos = 2
    ) %>% 
    gtable_add_cols(
      widths = spacer_width,
      pos = 4
    ) %>% 
    # grobs
    gtable_add_grob(
      grobs = p_table,
      t = 4, l = 4,
      name = "p_table"
    ) %>%
    gtable_add_grob(
      grobs = list(ylab_grob, label_table, xlab_grob),
      t = c(2, 2, 2), 
      l = c(2, 4, 6),
      name = "label_table"
    ) %>% 
    gtable_add_grob(
      grobs = size_table,
      t = 4, l = 6,
      name = "size_guide"
    )
    
  
  # p_out %>% gtable_show_layout()
  # p_out %>% grid.draw()
  
  return(p_out)  
}
