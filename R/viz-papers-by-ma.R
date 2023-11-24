build_papers_by_ma <- function(
    synthesis,
    papers_by_metanalysis_binwidth = 30,
    text_shrink_plot = .8
) {
  line_width <- line_width/2
  text_size_plot <- text_size_plot*text_shrink_plot
  synthesis <- 
    synthesis %>% 
    distinct(doi, nb_of_papers)
  
  synthesis_clean <- 
    synthesis %>% 
    distinct(doi, nb_of_papers)
      
  median_papers_by_metanalysis <- 
    synthesis_clean %>% 
    pull(nb_of_papers) %>% 
    median(na.rm = T)
  
  top_bin <- 
    synthesis_clean %>% 
    pull(nb_of_papers) %>% 
    chop_width(width = papers_by_metanalysis_binwidth, start = 0) %>% 
    table() %>%
    max()
  
  p <-
    synthesis_clean %>% 
    ggplot() +
    aes(x = nb_of_papers) +
    geom_histogram(fill = fill_color,
                   colour = 'black',
                   size = line_width,
                   binwidth = papers_by_metanalysis_binwidth,
                   boundary = .999) +
    annotate(
      geom = "text",
      x = median_papers_by_metanalysis,
      y = top_bin,
      colour = "black",
      size = text_size_plot,
      hjust = 0,
      vjust = 0,
      label = glue(' ←')
    ) +
    annotate(
      geom = "text",
      x = median_papers_by_metanalysis + 40,
      y = top_bin,
      colour = "black",
      size = text_size_plot,
      hjust = 0,
      vjust = .5,
      lineheight = lineheight,
      label = glue(
        '{median_papers_by_metanalysis} primary ',
        'studies synthetized by meta-analysis in median.') %>% 
        str_wrap(40)
    ) +
    geom_vline(xintercept = median_papers_by_metanalysis,
               linewidth = line_width,
               linetype = '22') +
    labs(x = "Primary studies synthetized ",
         y = "Selected meta-analyses") +
    scale_x_continuous(
      breaks = function(lim) {seq(0, lim[2], by = papers_by_metanalysis_binwidth*3)},
      expand = expansion(mult = c(.01, .1))
    ) +
    scale_y_continuous(
      breaks = function(lim) {seq(0, lim[2], by = 20)},
      expand = expansion(mult = c(0, .1))
    ) +
    theme(
      axis.line = element_line(linewidth = line_width),
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}
