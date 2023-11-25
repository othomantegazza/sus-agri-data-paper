build_p_screening_by_year <- function(
    screening,
    screening_dates,
    text_scaler = .8,
    cutoff_year = 2004,
    highlight_colour = "#e50de1"
) {
  

  # define scales and vars ----------------------------------------
  text_size_plot <- text_size_plot*text_scaler
  base_size <- base_size*text_scaler
  scale_fill_in_use <- 
    scale_fill_viridis_c(
      direction = -1,
      option = 'G',
      na.value = '#00000000',
      guide = guide_colourbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(5, "cm"),
        barheight = unit(.3, "cm")
      )
    )
  

  # prepare data -----------------------------------------------------
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


  # define base plot ----------------------------------------
  p <-
    screening %>%
    ggplot() +
    aes(x = year,
        y = fpid,
        fill = n)
 
  # legend guide --------------------------------------------------
  p_legend <-
    p + 
    geom_tile() +
    scale_fill_in_use +
    labs(fill = "Number of meta-analyses retrieved") +
    theme(
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size,
                                  vjust = 1),
      legend.position = "top"
    )
  
  p_legend <-
    p_legend %>% 
    ggplotGrob() %>% 
    gtable_filter("guide") %>% 
    .$grob %>% 
    .[[1]] %>% 
    gtable_filter("guide") %>% 
    .$grob %>% 
    .[[1]] %>% 
    gtable_filter("bar|label|ti") 
  
  # main plot -----------------------------------------------------
  p <- 
    p +
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
      size = text_size_plot,
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
    scale_fill_in_use +
    scale_colour_manual(
      values = c('white',
                 'black') %>%
        set_names()
    ) +
    theme(
      axis.line = element_blank(),
      axis.text.y = element_text(vjust = 0.5,
                                 colour = "black",
                                 size = base_size),
      axis.text.x.top = element_text(
        angle = 270,
        hjust = 1,
        vjust = .5,
        size = base_size
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
  

  # extract essential gtable from main ----------------------------
  p_minimal <- 
    p %>% 
    ggplotGrob() %>% 
    gtable_filter(pattern = "panel|axis|background") %>% 
    gtable_add_cols(
      widths = unit(0.13, "null"),
      pos = 4
      ) 


  # define table --------------------------------------------------
  p_table <-
    screening %>% 
    distinct(fpid, date_of_search) %>% 
    ggplot() +
    aes(x = 1,
        y = fpid) +
    geom_text(
      aes(label = date_of_search),
      size = text_size_plot,
      hjust = 1,
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      expand = expansion(0)
    ) +
    theme(panel.grid = element_blank())
  
  p_table_minimal <- 
    p_table %>% 
    ggplotGrob() %>% 
    gtable_filter(pattern = "panel")
  
  p_table_grob <- 
    p_table_minimal %>% 
    .$grob %>% 
    .[[1]]
  

  # text labels ---------------------------------------------------
  text_fpid <-  
    textbox_grob(
      text = "Farming Practice:",
      height = unit(1, "npc"),
      halign = 1,
      valign = 0,
      gp =gpar(
        fontsize = base_size,
        fontface = "italic"
      ),
      box_gp = gpar(
        fill = "#FFFFFF00",
        col = "#FFFFFF00"
      )
    )
  
  p_legend_2 <-
    gtable() %>%
    gtable_add_rows(
      heights = p_legend %>% gtable_height(),
      pos = 1
    ) %>% 
    gtable_add_rows(
      heights = unit(1, "null"),
      pos = 2
    ) %>% 
    gtable_add_cols(
      widths = unit(1, "null"),
      pos = 1
    ) %>% 
    gtable_add_cols(
      widths = p_legend %>% gtable_width(),
      pos = 2
    ) %>%
    gtable_add_grob(
      p_legend, t = 1, b = 1, l = 2, r = 2
    ) %>% 
    gtable_add_grob(
      text_fpid, t = 2, b = 2, l = 2, r = 2
    ) 

  text_date <- 
    textbox_grob(
      text = "Search<br>Date:",
      x = 1,
      y = 0,
      hjust = 1,
      halign = 1,
      vjust = 0,
      gp =gpar(
        fontsize = base_size,
        fontface = "italic"
      )
    )
  
  text_year <- 
    textbox_grob(
      text = "Meta-analysis publication year",
      x = 1,
      hjust = 1,
      halign = 1,
      y = .55,
      vjust = .5,
      gp =gpar(
        fontsize = base_size,
        fontface = "italic"
      )
    )
  

  # put everything ------------------------------------------------
  p_out <- 
    p_minimal %>% 
    gtable_add_grob(text_date, t = 6, l = 5) %>% 
    gtable_add_grob(text_year, t = 6, l = 6) %>% 
    gtable_add_grob(p_table_grob, t = 7, l = 5) %>% 
    gtable_add_grob(p_legend_2, t = 6, l = 4, z = 2) 
  
  return(p_out)
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