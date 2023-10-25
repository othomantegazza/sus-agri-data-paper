bar_styles <-  geom_col(
    fill = 'white',
    colour = 'black',
    size = line_width/2,
    width = 1
  ) 

build_p4 <- function(synthesis,
                     text_offset = .01,
                     y_offset = 5) {
  browser()
  
  to_plot <- 
    synthesis %>% 
    count(impact_matrix,
          sort = T)
  
  max_x <- 
    to_plot$n %>% 
    max()
  
  to_plot %>% 
    ggplot() +
    aes(
      y = impact_matrix %>% 
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
}