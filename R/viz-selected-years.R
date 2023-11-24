build_selected_years <- function(
    ma_list,
    screening_dates
) {
  line_width <- line_width/2
  text_size_plot <- text_size_plot*.8
  
  cutoff_date <- 
    screening_dates %>% 
    pull(date_of_search) %>% 
    min(na.rm = T) 
  
  cutoff_position <- 
    yday(cutoff_date) /
    ((cutoff_date %>% ceiling_date(unit = "years") - 1) %>% yday()) 
  
  cutoff_position <- cutoff_position + year(cutoff_date)

  p <-
    ma_list %>% 
    ggplot() +
    aes(
      x = year,
    ) +
    geom_histogram(
      binwidth = 1,
      fill = fill_color,
      colour = 'black',
      size = line_width,
      boundary = .999
    ) +
    annotate(geom = "segment",
             x = cutoff_position,
             xend = cutoff_position,
             y = 0,
             yend = 140,
             colour = 'black',
             size = line_width,
             linetype = "22"
    ) +
    geom_hline(yintercept = 0,
               colour = 'black',
               size = line_width*2
    ) +
    annotate(
      geom = "text",
      label = glue("First search: {cutoff_date}"),
      y = 145,
      x = cutoff_position,
      size = text_size_plot
    ) +
    annotate(
      geom = "text",
      label = " →",
      y = 130,
      x = cutoff_position,
      hjust = 0,
      size = text_size_plot
    ) +
    annotate(
      geom = "text",
      label = "Collection\nongoing",
      y = 130,
      x = cutoff_position + 1.2,
      hjust = 0,
      size = text_size_plot,
      lineheight = .8
    ) +
    annotate(
      geom = "text",
      label = "Collection done",
      y = 130,
      x = cutoff_position - 1.2,
      hjust = 1,
      size = text_size_plot
    ) +
    annotate(
      geom = "text",
      label = "← ",
      y = 130,
      x = cutoff_position,
      hjust = 1,
      size = text_size_plot
    ) +
    labs(
      linetype = 'Collection status',
      x = "Publication year",
      y = 'Selected meta-analysis'
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, .1))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1)),
      breaks = ~seq(.[1], .[2], by = 20)
    ) +
    theme(
      axis.line = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}
