# make_card <- function(
#     n,
#     text_label,
#     title_size
# ) {
#   browser()
#   textbox_grob(
#     text = text_label, 
#     x = 0,
#     y = 1,
#     hjust = 0,
#     vjust = 1, 
#     gp = gpar(
#       fontsize = title_size
#     ) 
#   )  
# }

build_prisma_page <- function(
    fpid,                 
    papers_after_search,  
    papers_after_merging,
    B,
    O,
    R,
    ...
) {
  browser()
  
  title_size <- 24
  body_size <- title_size/2
  
  heights <- c(1, 2, 4, 8, 4, 1)
  widths <- c(1, 13, 1)
  
  text_cell_width <- 5
  
  # layouts -------------------------------------------------------
  
  diagr <- 
    gtable() %>% 
    gtable_add_rows(heights = unit(heights, "cm")) %>% 
    gtable_add_cols(widths = unit(widths, "cm"))
  
  
  search_layout <- 
    gtable() %>% 
    gtable_add_rows(
      heights = unit(c(title_size, title_size*2), "points")
      ) %>% 
    gtable_add_rows(
      heights = unit(1, "null")
    ) %>% 
    gtable_add_cols(widths = unit(c(5, 3, 5), "cm"))
  
  search_layout %>% gtable_show_layout()
  
  
  # arrows --------------------------------------------------------
  h_arrow <-
    linesGrob(
      x = unit(c(0, .9), 'npc'),
      y = unit(rep(.9, 2), 'npc'),
      arrow = arrow(
        length = unit(.2, 'cm'),
        type = "open"
      ),
      gp = gpar(
      )
  )
  

  # title ---------------------------------------------------------
  
  title <- textbox_grob(
    text = fpid, 
    x = 0,
    y = 1,
    hjust = 0,
    vjust = 1, 
    gp = gpar(
      fontsize = title_size
    ) 
  )
  
  # search --------------------------------------------------------

  text_search <- "MAs after querying WOS and SCOPUS."
  text_search_excluded <- 
    "MAs after removing duplicates from the two searches."
  
  search_n <- 
    textbox_grob(
      text = papers_after_search, 
      x = 0,
      y = 1,
      hjust = 0,
      vjust = 1, 
      gp = gpar(
        fontsize = title_size
      ) 
  )
  
  search_excluded_n <- 
    textbox_grob(
      text = papers_after_search - papers_after_merging, 
      x = 0,
      y = 1,
      hjust = 0,
      vjust = 1, 
      gp = gpar(
        fontsize = title_size
      ) 
    )
  
  search_text_grob <- 
    textbox_grob(
      text = text_search, 
      x = 0,
      y = 1,
      hjust = 0,
      vjust = 1, 
      gp = gpar(
        fontsize = body_size
      ) 
    )
  
  search_excluded_text_grob <- 
    textbox_grob(
      text = text_search_excluded, 
      x = 0,
      y = 1,
      hjust = 0,
      vjust = 1, 
      gp = gpar(
        fontsize = body_size
      ) 
    )
  
  search_diagr <- 
    search_layout %>% 
    gtable_add_grob(search_n, t = 1, l = 1) %>% 
    gtable_add_grob(search_excluded_n, t = 1, l = 3) %>% 
    gtable_add_grob(search_text_grob, t = 2, l = 1) %>% 
    gtable_add_grob(h_arrow, t = 2, l = 2) %>% 
    gtable_add_grob(search_excluded_text_grob, t = 2, l = 3)
  
  
  search_diagr %>% gtable_show_layout()
  search_diagr %>% grid.draw()
  
  # downloaded <- 
  #   make_card(n = papers_after_search,
  #             text_label = text_search,
  #             title_size = title_size)
  

  # all -----------------------------------------------------------
  
  diagr_out <- 
    diagr %>% 
    gtable_add_grob(title, t = 2, l = 2) %>% 
    gtable_add_grob(search_diagr, t = 3, l = 2)
  
  grid.newpage()
  diagr %>% gtable_show_layout()
  diagr_out %>% grid.draw()
}
  
build_p_prisma <- function(search_tab,
                           screening) {
  # browser()
  
  search_tab <- 
    search_tab %>% 
    transmute(
      fpid,
      papers_after_search = nb_papers_scopus + nb_papers_wos,
      papers_after_merging = nb_papers_after_merging
    ) 
  
  screening <- 
    screening %>% 
    count(FPID, Status) %>% 
    drop_na() %>% 
    pivot_wider(
      names_from = Status,
      values_from = n
    )
  
  prisma_d <- 
    full_join(
      search_tab,
      screening,
      by = c(
        "fpid" = "FPID"
      )
    )
  
  screening_gtable <- 
    
  
  prisma_d %>% 
    pmap(
      build_prisma_page
      )
  
}
