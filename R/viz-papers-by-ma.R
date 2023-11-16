build_papers_by_ma <- function(
    synthesis,
    papers_by_metanalysis_binwidth = 25
) {
  median_papers_by_metanalysis <- 
    synthesis %>% 
    pull(nb_of_papers) %>% 
    median(na.rm = T)
  
  p <-
    synthesis %>% 
    ggplot() +
    aes(x = nb_of_papers) +
    geom_histogram(fill = fill_color,
                   colour = 'black',
                   size = line_width,
                   binwidth = papers_by_metanalysis_binwidth) +
    stat_bin(
      binwidth = papers_by_metanalysis_binwidth,
      fill = fill_color
    ) +
    stat_bin(
      aes(y = ifelse(
        {
          after_stat(count) < after_stat(max(count))
        },
        NA,
        after_stat(count))
      ),
      binwidth = papers_by_metanalysis_binwidth,
      colour = "black",
      size = base_size/size_scale,
      hjust = -.1,
      vjust = -1,
      geom = "text",
      label = glue('Median: {median_papers_by_metanalysis} papers by meta-analysis.')
    ) +
    geom_vline(xintercept = median_papers_by_metanalysis,
               linewidth = line_width/2,
               linetype = '11') +
    labs(x = "Number of paper analyzed for each meta-analysis [n]",
         y = "Number of meta-analyses [n]") +
    scale_x_continuous(
      breaks = function(lim) {seq(0, lim[2], by = 100)},
      expand = expansion(mult = c(0, .1))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    )
  
  return(p)
}
