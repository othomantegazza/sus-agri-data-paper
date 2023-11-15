p4_basic <- function(synthesis,
                     var_in,
                     text_offset = .01,
                     y_offset = 20, 
                     max_rows = 50) {

  to_plot <- 
    synthesis %>% 
    rename(y_var = {{var_in}}) %>% 
    count(y_var) %>% 
    arrange(desc(n))
  
  max_x <- 
    to_plot$n %>% 
    max()

  if(nrow(to_plot) > max_rows) {
    to_plot <- 
      to_plot %>% 
      slice(1:max_rows)
  }
  

  p <- 
    to_plot %>% 
    ggplot() +
    aes(
      y = y_var %>% 
        as_factor() %>% 
        fct_rev,
      x = n
    ) +
    geom_col(
      fill = 'white',
      colour = 'black',
      size = line_width/2,
      width = 1
    ) +
    geom_text(
      aes(
        x = n + max_x * text_offset,
        label = n
      ),
      size = text_size_plot,
      hjust = 0
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, text_offset*y_offset))
    ) + 
    geom_vline(
      xintercept = 0,
      size = line_width
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.spacing = unit(0, 'mm'),
      strip.text = element_blank()
    )
  
  return(p)
}

build_p4 <- function(synthesis, pico_results) {
  browser() 
  
  p_fpid <- 
    synthesis %>% 
    select(doi, fpid) %>% 
    distinct() %>% 
    p4_basic(var_in = "fpid") %>% 
    ggplotGrob() 
  
  tst <- 
  p_fpid %>% 
    gtable_filter('panel') %>% .$grob 
    grid.draw()
  
  p_fpid %>% 
    gtable_filter('axis-l') %>% 
    # gtable_show_layout()
    grid.draw()
  
  p_impact <-
    synthesis %>% 
    p4_basic(var_in = "impact_matrix")
  
  p_metrics <- 
    pico_results %>% 
    p4_basic(var_in = "metric_matrix")
}
