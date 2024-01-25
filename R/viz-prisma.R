tgrob <- function(text, size) {
  textbox_grob(
    text = text, 
    x = 0,
    y = 1,
    hjust = 0,
    vjust = 1, 
    gp = gpar(
      fontsize = size
    ) 
  )
}

build_prisma_page <- function(
    fpid,
    papers_after_search,
    discarded_after_merging,   
    screened,
    discarded_after_screening,
    evaluated,
    discarded_after_evaluation,
    retained,  
    ...
) {
  # browser()
  
  title_size <- 35
  body_size <- title_size/2
  
  heights <- 
    c(1, 2, 4, 4, 4, 4, 1) %>% 
    {
      scales::rescale(
        .,
        to = c(0, a4_height/10),
        from = c(0, sum(.))
      )
    }
  
  widths <- 
    c(1, 13, 1) %>% 
    {
      scales::rescale(
        .,
        to = c(0, a4_width/10),
        from = c(0, sum(.))
      )
    }
  
  text_cell_width <- 5
  
  inner_widths <- 
    c(5, 3, 5) %>% {
      scales::rescale(
        .,
        to = c(0, widths[2]),
        from = c(0, sum(.))
      )
    }
  
  inner_widths <- unit(inner_widths, "cm")
  inner_heights <- unit(c(title_size, title_size*2), "points")
  
  text_search <- 
    "Records after querying the WOS and SCOPUS databases."
  
  text_search_excluded <- 
    "Removed duplicated records"
  
  # layouts -------------------------------------------------------
  
  diagr <- 
    gtable() %>% 
    gtable_add_rows(heights = unit(heights, "cm")) %>% 
    gtable_add_cols(widths = unit(widths, "cm"))
  
  
  search_layout <-  
    gtable() %>% 
    gtable_add_rows(
      heights = inner_heights
      ) %>% 
    gtable_add_rows(
      heights = unit(1, "null")
    ) %>% 
    gtable_add_cols(widths = inner_widths)
  
  # arrows --------------------------------------------------------
  h_arrow <-
    linesGrob(
      x = unit(c(.05, .9), 'npc'),
      y = unit(rep(.9, 2), 'npc'),
      arrow = arrow(
        length = unit(.2, 'cm'),
        type = "open"
      ),
      gp = gpar(
      )
  )
  
  v_arrow <-
    linesGrob(
      y = unit(c(1, .1), 'npc'),
      x = unit(rep(.1, 2), 'npc'),
      arrow = arrow(
        length = unit(.2, 'cm'),
        type = "open"
      ),
      gp = gpar(
      )
    )

  # title ---------------------------------------------------------
  
  title <- tgrob(fpid, title_size)
  
  # search --------------------------------------------------------

  search_n <- tgrob(papers_after_search, title_size)
  
  search_excluded_n <- 
    tgrob(
      discarded_after_merging,
      title_size
    ) 
  
  search_text_grob <- tgrob(text_search, body_size)
  
  search_excluded_text_grob <- 
    tgrob(text_search_excluded, body_size)
  
  search_diagr <- 
    search_layout %>% 
    gtable_add_grob(search_n, t = 1, l = 1) %>% 
    gtable_add_grob(search_excluded_n, t = 1, l = 3) %>% 
    gtable_add_grob(search_text_grob, t = 2, l = 1) %>% 
    gtable_add_grob(h_arrow, t = 2, l = 2) %>% 
    gtable_add_grob(search_excluded_text_grob, t = 2, l = 3) %>% 
    gtable_add_grob(v_arrow, t = 3, l = 1)
    

  # screening -----------------------------------------------------
  
  screened_n <- tgrob(screened, title_size)
  screened_n_text <- tgrob(
    "Record Screened.", body_size
  )
  screened_excluded_n <- tgrob(discarded_after_screening, title_size)
  screened_excluded_n_text <- tgrob(
    "Record excluded after screening.", body_size
  )
  
  screening_diagr <- 
    search_layout %>% 
    gtable_add_grob(screened_n, t = 1, l = 1) %>% 
    gtable_add_grob(screened_excluded_n, t = 1, l = 3) %>% 
    gtable_add_grob(screened_n_text, t = 2, l = 1) %>% 
    gtable_add_grob(h_arrow, t = 2, l = 2) %>% 
    gtable_add_grob(screened_excluded_n_text, t = 2, l = 3) %>% 
    gtable_add_grob(v_arrow, t = 3, l = 1)

  # elegibility ---------------------------------------------------
  
  elegibility_n <- tgrob(evaluated, title_size)
  elegibility_n_text <- tgrob(
    "Record assessed for elegibility.", body_size
  )
  elegibility_excluded_n <- tgrob(discarded_after_evaluation, title_size)
  elegibility_excluded_n_text <- tgrob(
    "Record excluded after elegibility assessment.", body_size
  )
  
  elegibility_diagr <- 
    search_layout %>% 
    gtable_add_grob(elegibility_n, t = 1, l = 1) %>% 
    gtable_add_grob(elegibility_excluded_n, t = 1, l = 3) %>% 
    gtable_add_grob(elegibility_n_text, t = 2, l = 1) %>% 
    gtable_add_grob(h_arrow, t = 2, l = 2) %>% 
    gtable_add_grob(elegibility_excluded_n_text, t = 2, l = 3) %>% 
    gtable_add_grob(v_arrow, t = 3, l = 1)
  
  
  # included ------------------------------------------------------
  
  included_n <- tgrob(retained, title_size)
  included_n_text <- tgrob(
    "Record included in the review.", body_size
  )
  
  included_diagr <- 
    search_layout %>% 
    gtable_add_grob(included_n, t = 1, l = 1) %>% 
    gtable_add_grob(included_n_text, t = 2, l = 1)
  
  # all -----------------------------------------------------------
  
  diagr_out <- 
    diagr %>% 
    gtable_add_grob(title, t = 2, l = 2) %>% 
    gtable_add_grob(search_diagr, t = 3, l = 2) %>% 
    gtable_add_grob(screening_diagr, t = 4, l = 2) %>% 
    gtable_add_grob(elegibility_diagr, t = 5, l = 2) %>% 
    gtable_add_grob(included_diagr, t = 6, l = 2) 

  return(diagr_out)
}
  
build_p_prisma <- function(search_tab,
                           screening) {
  
  # browser()
  
  # prepare search data -------------------------------------------
  
  search_tab <-
    search_tab %>% 
      rowwise() %>% 
    transmute(
      fpid,
      papers_after_search = sum(nb_papers_scopus, nb_papers_wos, na.rm = T),
      papers_after_merging = nb_papers_after_merging
    ) %>% 
      mutate(
        papers_after_search = papers_after_search %>% 
          {
            case_when(. == 0 ~ NA,
                      TRUE ~ .)
          }
      )
  
  # prepare screening tab -----------------------------------------
  
  screening <-
    screening %>% 
    count(fpid, status) %>% 
    drop_na() %>% 
    pivot_wider(
      names_from = status,
      values_from = n
    )
  
  prisma_d <- 
    full_join(
      search_tab,
      screening,
      by = c(
        "fpid" = "fpid"
      )
    ) %>% 
    rowwise() %>% 
    select(-papers_after_merging) %>% 
    mutate(
      screened = sum(B, O, R),
      .after = papers_after_search
      ) %>% 
    mutate(
      discarded_after_merging = sum(
        papers_after_search, - screened
      ),
      .after = papers_after_search
    ) %>% 
    mutate(
      discarded_after_screening = R,
      evaluated = O + B,
      discarded_after_evaluation = O,
      retained = B,
      .after = screened
    ) %>% 
    select(-B, -O, -R)
  
  
  prisma_p <- 
    prisma_d %>% 
    pmap(
      build_prisma_page
    )
  
  return(prisma_p)
}
