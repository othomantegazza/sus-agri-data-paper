build_p3 <- function(ma_list,
                     cutoff_year = 2021) {
  ma_list <-  
    ma_list %>% 
    mutate(
      ongoing = if_else(
        condition = year >= cutoff_year,
        true = 'ongoing',
        false = 'done'
      )
    )
  
  p <- 
    ma_list %>% 
    ggplot() +
    aes(x = year,
        linetype = ongoing) +
    geom_histogram(
      binwidth = 1,
      fill = 'white',
      colour = 'black',
      size = line_width/2,
    ) +
    scale_linetype_manual(
      values = c(
        done = 'solid',
        ongoing = '22'
      )
    ) +
    labs(
      linetype = 'Collection status',
      y = 'Number of MAs'
    ) +
    scale_x_continuous(
      expand = expansion(0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    ) +
    theme(
      axis.line = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}
