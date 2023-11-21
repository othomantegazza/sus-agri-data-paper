build_p_screening_by_year <- function(
    screening,
    screening_dates,
    text_scaler = .8,
    cutoff_year = 2004,
    highlight_colour = "#e50de1"
) {
  screening_dates <-
    screening_dates %>%
    arrange(date_of_search)
  
  screening <-
    screening %>%
    mutate(
      year = year %>% {
        case_when(. <= cutoff_year ~ cutoff_year,
                  TRUE ~ .)
      }
    ) %>% 
    count(fpid,  year) %>% 
    left_join(
      screening_dates,
      by = "fpid"
    ) %>% 
    mutate(
      is_year_of_search = case_when(
        year(date_of_search) == year ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(year = year %>% {
      case_when(. <= cutoff_year ~ paste(cutoff_year, "or earlier"),
                TRUE ~ as.character(.))
    }) %>% 
    arrange(year) %>% 
    mutate(year = as_factor(year)) %>% 
    arrange(date_of_search) %>% 
    mutate(fpid = fpid %>% as_factor() %>% fct_rev()) %>% 
    drop_na(year)
  
  p <-
    screening %>%
    ggplot() +
    aes(x = year,
        y = fpid,
        fill = n) +
    geom_tile(
      aes(colour = is_year_of_search),
      size = .8,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = c(
        '#FFFFFF00',
        highlight_colour
      )
    ) +
    new_scale_color() +
    geom_text(
      aes(label = n,
          colour = case_when(
            n > 30 ~ 'white',
            TRUE ~ 'black'
          )
      ),
      size = text_size_plot*text_scaler,
      show.legend = FALSE
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    guides(
      fill = 'none'
    ) +
    scale_x_discrete(
      position = 'top'
    ) +
    scale_fill_viridis_c(
      direction = -1,
      option = 'G',
      na.value = '#00000000',
    ) +
    scale_colour_manual(
      values = c('white',
                 'black') %>%
        set_names()
    ) +
    theme(
      axis.line = element_blank(),
      axis.text.y = element_text(vjust = 0.5,
                                 colour = "black",
                                 size = base_size*text_scaler),
      axis.text.x.top = element_text(
        angle = 90,
        hjust = 0,
        vjust = .5,
        size = base_size*text_scaler
      ),
      axis.ticks = element_line(
        size = line_width*.2
      ),
      panel.grid = element_line(
        colour = 'black',
        size = line_width*.1,
        linetype = '11'
      )
    )
  
  return(p)
}




# 
# # viz -----------------------------------------------------------
# 
# plot_screening_year <- function(part) {
#   p <-
#     systematic_screening_clean %>% 
#     filter(
#       fpid %in% screening_dates$data[[part]]$fpid
#     ) %>% 
#     left_join(
#       screening_dates$data[[part]],
#       by = c("fpid" = "fpid")
#     ) %>% 
#     mutate(
#       fpid = paste(
#         date_of_search, 
#         fpid %>% str_wrap(width = 20),
#         sep = "\n"
#       )
#     ) %>% 
#     count(fpid, year) %>%
#     complete(fpid, year, fill = list(n = 0)) %>% 
#     ggplot() +
#     aes(x = year,
#         y = n) +
#     geom_col(
#       fill = 'black',
#       colour = 'black',
#       size = line_width/2,
#       width = 1
#     ) +
#     geom_hline(yintercept = 0, 
#                # linewidth = 0,
#                colour = "white") +
#     facet_wrap(
#       facets = "fpid",
#       ncol = 1, 
#       strip.position = "left"
#       # labeller = label_wrap_gen(width = 15)
#     ) +
#     labs(
#       x = NULL,
#       y = NULL
#     ) +
#     scale_y_continuous(position = "right",
#                        breaks = c(0, 50, 100), 
#                        limits = c(0, NA)) +
#     theme(
#       axis.line.y = element_blank(),
#       # panel.grid.major.x = element_blank(),
#       axis.line.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.text.x = element_text(
#         angle = 90,
#         hjust = 1,
#         vjust = .5
#       ),
#       axis.text.y = element_text(
#         vjust = 0
#       ),
#       panel.spacing = unit(0, 'mm'),
#       strip.text.y.left = element_text(
#         angle = 0,
#         hjust = 1,
#         vjust = 0
#       )
#     ) 
#   return(p)
# }
# 
# p1 <- plot_screening_year(1) %>% ggplotGrob()
# p2 <- plot_screening_year(2) %>% ggplotGrob()
# 
# p <- 
#   gtable() %>% 
#   gtable_add_rows(heights = unit(25, "cm")) %>% 
#   gtable_add_cols(widths = unit(c(10, 10), "cm")) %>% 
#   gtable_add_grob(grobs = p1, l = 1, t = 1) %>%
#   gtable_add_grob(grobs = p2, l = 2, t = 1)
# 
# p %>% gtable_show_layout()
# 
# grid.newpage();grid.draw(p)
# 
# 
# # 2nd version