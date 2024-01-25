make_diagram <- function(
    short_description,
    n,
    discarded,
    top_arrow = T,
    right_arrow = T,
    top_text = T,
    text_height = 2,
    text_widths = 1.7,
    ...
) {
  # layout --------------------------------------------------------
  
  gp <- gpar(
    fontsize = base_size
  )
  
  hs <- c(2, .2, 1, text_height)
  ws <- c(text_widths, 1, .2)
  
  diagr <- 
    gtable() %>% 
    gtable_add_rows(
      heights = unit(hs[1], "cm")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[2], "cm")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[3], "null")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[4], "cm")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[1], "cm")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[2], "null")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[3], "cm")
    )
  
  # check_plot(diagr)
  
  # bottom text
  btxt <- 
    textbox_grob(
      text = paste(
        short_description, n, ' MAs', sep = ''
      ),
      x = unit(0, "npc"),
      y = unit(0, "npc"),
      width = unit(1, "npc"),
      hjust = 0,
      vjust = 0,
      gp = gp
    )
  
  diagr <- 
    diagr %>% 
    gtable_add_grob(
      grobs = btxt,
      t = 4,
      l = 1
    ) 
  
  # check_plot(diagr)
  
  if(top_text) {
    ttxt <- 
      textbox_grob(
        text = ,paste(
          'Discarded:', ' ', discarded, ' ', 'MAs', sep = ''
        ),
        x = unit(0, "npc"),
        y = unit(0, "npc"),
        width = unit(1, "npc"),
        hjust = 0,
        vjust = 0,
        gp = gp,
        box_gp = gpar(fill = "white",
                      col = '#00000000')
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = ttxt,
        t = 1,
        l = 1
      )
  }
  
  if(right_arrow) {
    harr <- 
      linesGrob(
        x = unit(c(0, 1), 'npc'),
        y = unit(rep(base_size, 2), 'points'),
        arrow = arrow(
          length = unit(.2, 'cm'),
          type = "open"
        ),
        gp = gpar(
          lty = '11'
        )
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = harr,
        t = 4,
        l = 2
      )
  }
  
  if(top_arrow) {  
    varr <- 
      linesGrob(
        x = unit(rep(base_size, 2), 'points'),
        y = unit(c(0, 1), 'npc'),
        arrow = arrow(
          length = unit(.2, 'cm'),
          type = "open"
        ),
        gp = gpar(
          lty = '11'
        )
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = varr,
        t = 3,
        l = 1
      )
  }
  return(diagr)
}

build_p_screening <- function(
    screening,
    fill_color = "white",
    text_scaler = .9,
    lab_y = "↓ Farming practice categories:"
)
{
  # browser()
  
  # basic setup ---------------------------------------------------
  
  text_size_plot <- text_size_plot*text_scaler
  base_size <- base_size*text_scaler
  
  if(
    any(
      is.na(
        screening$status
      )
    )
  ) {
    warning("Dropping Papers with Missing status")
  }
  
  # prepare data --------------------------------------------------
  
  status_by_fpid <- 
    screening %>% 
    count(fpid, status, status_details) %>% 
    drop_na()
  
  fpid_ordered_screening <- 
    status_by_fpid %>% 
    summarise(
      n = n %>% sum(na.rm = T),
      .by = fpid
    ) %>% 
    arrange(desc(n)) %>% 
    pull(fpid) 
  
  screening_levels <- 
    c(
      'From WOS and or Scopus searches: retrieved ',
      'After Reading Abstract: retained ',
      'After Reading Full Text: retained '
    )
  
  status_by_fpid <- 
    status_by_fpid %>% 
    mutate(
      short_description = status %>% {
        case_when(
          . == 'R' ~ screening_levels[1],
          . == 'O' ~ screening_levels[2],
          . == 'B' ~ screening_levels[3]
        )
      } %>% 
        factor(levels = screening_levels)
    )
  
  # main plot -----------------------------------------------------
  
  p_screening <- 
    status_by_fpid %>% 
    arrange(fpid, status) %>% 
    mutate(tot_before_step = n %>% cumsum(),
           .by = fpid) %>% 
    ggplot() +
    aes(
      x = tot_before_step,
      y = fpid %>%
        factor(levels = fpid_ordered_screening) %>%
        fct_rev()) +
    geom_col(
      fill = fill_color,
      colour = 'black',
      size = line_width/2,
      width = 1
    ) +
    geom_text(
      aes(
        x = tot_before_step + 10,
        label = tot_before_step
      ),
      size = text_size_plot,
      hjust = 0
    ) +
    geom_vline(
      xintercept = 0,
      size = line_width
    ) +
    facet_grid(
      . ~ short_description,
      scales = 'free_x',
      space = 'free_x',
      labeller = label_wrap_gen(width = 12)
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      expand = expansion(add = c(0, 150)),
      limits = ~c(0, ifelse(.[2] > 150, .[2], 150))
    ) +
    theme(
      axis.line.y = element_blank(),
      axis.text.y = element_text(vjust = 0.5,
                                 colour = "black",
                                 size = base_size),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.spacing = unit(0, 'mm'),
      strip.text = element_blank()
    )
  
  # collect statuses for top panel---------------------------------
  
  statuses <- 
    status_by_fpid %>% 
    group_by(status, short_description) %>% 
    summarise(n = n %>% sum()) %>% 
    ungroup() %>% 
    arrange(status) %>% 
    mutate(n = cumsum(n)) %>% 
    arrange(desc(status)) %>% 
    mutate(discarded = c(NA, n[1:{n() - 1}]) - n) %>% 
    mutate(
      top_arrow = c(F, T, T),
      right_arrow = c(T, T, F),
      top_text = top_arrow,
      text_height = c(3, 2, 2),
      text_widths = c(2.5, 1.7, 1.7) 
    )
  
  status_tables <- 
    statuses %>% pmap(make_diagram)
  
  # text labels ---------------------------------------------------
  text_fpid <-  
    textbox_grob(
      text = lab_y,
      height = unit(1, "npc"),
      halign = 1,
      valign = 0,
      margin = unit(c(0, 1.5, 0, 0), "mm"),
      gp =gpar(
        fontsize = base_size,
        fontface = "italic"
      ),
      box_gp = gpar(
        fill = "#FFFFFF00",
        col = "#FFFFFF00"
      )
    )
  
  # put everything together ---------------------------------------
  
  p_screening_augmented <- 
    p_screening %>%
    ggplotGrob() %>% 
    # keep only panels and the active axis
    gtable_filter(
      pattern = c('panel|axis-l-1')
    ) %>% 
    # add padding
    gtable_add_rows(
      heights = unit(5.5, "cm"),
      pos = 0
    ) %>% 
    gtable_add_grob(
      grobs = status_tables[[1]],
      t = 1,
      l = 2
    ) %>% gtable_add_grob(
      grobs = status_tables[[2]],
      t = 1,
      l = 4
    ) %>% gtable_add_grob(
      grobs = status_tables[[3]],
      t = 1,
      l = 6
    ) %>% 
    gtable_add_grob(
      grobs = text_fpid,
      t = 1, 
      l = 1
    ) %>% 
    gtable_add_rows(height = padding/2, pos = 1) %>% 
    gtable_add_padding(padding = padding) 
  
  # check_plot(p_screening_augmented)

  return(p_screening_augmented)
}
