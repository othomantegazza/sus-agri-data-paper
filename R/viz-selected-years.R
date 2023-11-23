build_selected_years <- function(
    ma_list,
    screening_dates
) {
  line_width <- line_width/2
  
  cutoff_year <- 
    screening_dates %>% 
    pull(date_of_search) %>% 
    min(na.rm = T) %>% 
    year()
  
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
      fill = fill_color,
      colour = 'black',
      size = line_width,
    ) +
    scale_linetype_manual(
      values = c(
        done = 'solid',
        ongoing = '22'
      )
    ) +
    labs(
      linetype = 'Collection status',
      x = "Publication year",
      y = 'Selected meta-analysis'
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
