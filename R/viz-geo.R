build_p_geo <- function(pico_combinations,
                        x_expansion = .15,
                        fill_color = "white") {
  
  geo <- 
    pico_combinations %>% 
    distinct(doi, geo_coverage) %>% 
    select(geo_coverage) %>% 
    mutate(
      geo_coverage = geo_coverage %>% str_split(pattern = ";")
    ) %>% 
    unnest(geo_coverage) %>%
    mutate(
      geo_coverage = geo_coverage %>% str_trim()
    ) %>% 
    count(geo_coverage, sort = T)
  
  p <- 
    geo %>% 
    mutate(
      geo_coverage = geo_coverage %>% as_factor() %>% fct_rev()
    ) %>% 
    ggplot() +
    aes(y = geo_coverage,
        x = n) +
    geom_col(width = 1,
             linewidth = line_width/2,
             colour = 'black',
             fill = fill_color) +
    geom_text(aes(label = n,
                  x = n + 8),
              hjust = 0,
              size = base_size/size_scale) +
    scale_x_continuous(
      expand = expansion(mult = c(0, x_expansion)),
      position = "top"
    ) +
    labs(x = "Papers [n]",
         y = "Geographical Coverage") +
    theme(
      axis.line.x = element_blank(),
      axis.line.y = element_line(linewidth = line_width),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(hjust = .5),
      axis.text.y = element_text(vjust = .5)
    )
  
  return(p)
}