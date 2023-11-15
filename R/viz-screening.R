build_p_screening <- function(
    screening,
    fill_color = "white"
)
{
  if(
    any(
      is.na(
        screening$status
      )
    )
  ) {
    warning("Dropping Papers with Missing status")
  }
  
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
      'After Web Search',
      'After Reading Abstract',
      'After Reading Full Text'
    )
  
  status_by_fpid <- 
    status_by_fpid %>% 
    mutate(
      short_description = status %>% {
        case_when(. == 'R' ~ screening_levels[1],
                  . == 'O' ~ screening_levels[2],
                  . == 'B' ~ screening_levels[3]
        )
      } %>% 
        factor(levels = screening_levels)
    )
  
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
      expand = expansion(add = c(0, 100)),
      limits = ~c(0, ifelse(.[2] > 150, .[2], 150))
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
      top_text = top_arrow
    )
  
  gp <- gpar(
    fontsize = base_size
  )
  
  make_diagram <- function(
    short_description,
    n,
    discarded,
    top_arrow = T,
    right_arrow = T,
    top_text = T,
    ...
  ) {
    hs <- c(2, .2, 1, 2.2, .2)
    ws <- c(2, 1, .5)
    
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
      gtable_add_rows(
        heights = unit(hs[5], "cm")
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
    
    # bottom text
    btxt <- 
      textbox_grob(
        text = paste(
          short_description, ': retained ', n, ' MAs', sep = ''
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
  
  status_tables <- 
    statuses %>% pmap(make_diagram)

  p_screening_augmented <- 
    p_screening %>%
    ggplotGrob() %>% 
    # keep only panels and the active axis
    gtable_filter(
      pattern = c('panel|axis-l-1')
    ) %>% 
    # add padding
    gtable_add_cols(
      widths = unit(.2, 'cm'),
      pos = 0
    ) %>% 
    gtable_add_cols(
      widths = unit(.5, 'cm')
    ) %>% 
    gtable_add_rows(
      heights = unit(.5, 'cm'),
      pos = 0
    ) %>% 
    gtable_add_rows(
      heights = unit(.5, 'cm')
    )  %>% 
    gtable_add_rows(
      heights = unit(5.5, "cm"),
      pos = 1
    ) %>% 
    gtable_add_grob(
      grobs = status_tables[[1]],
      t = 2,
      l = 3
    ) %>% gtable_add_grob(
      grobs = status_tables[[2]],
      t = 2,
      l = 5
    ) %>% gtable_add_grob(
      grobs = status_tables[[3]],
      t = 2,
      l = 7
    ) 

  return(p_screening_augmented)
}
