build_p_review_log <- function(
    review_log,
    lab_fill = "Values corrected",
    lab_y = "Farming practice categories",
    lab_x = "Data section ",
    margin_size = unit(.5, "cm"),
    text_scaler = .8,
    fontface = "italic"
    ) 
{
  browser()

  # setup ---------------------------------------------------------
  
  text_size_plot <- text_size_plot*text_scaler
  base_size <- base_size*text_scaler
  
  scale_fill_in_use <- 
    scale_fill_viridis_c(
      direction = -1,
      option = 'G',
      na.value = '#00000000',
      guide = guide_colourbar(
        title.theme = element_text(
          size = base_size,
          face = fontface),
        label.theme = element_text(
          size = base_size),
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(5, "cm"),
        barheight = unit(.2, "cm")
      )
    )
  
  # detect sections -----------------------------------------------
  
  all_sections <- 
    paste("Section", rep(1:10)) %>% 
    paste(collapse = ", ") 
  
  # order logs ----------------------------------------------------

  fpid_order <- 
    review_log %>% 
    summarise(n = n %>% sum(na.rm = T), 
              .by = fpid) %>% 
    arrange(n) %>% 
    pull(fpid)
  
  # define main plot ----------------------------------------------
  
  p <- 
    review_log %>%
    ggplot() +
    aes(x = section_of_dataset,
        y = fpid %>% factor(levels = fpid_order),
        fill = n) +
    geom_tile() +
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
      y = NULL,
      fill = lab_fill
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
  
  # extract sections ----------------------------------------------
  
  pgrob <- 
    p %>% ggplotGrob()
  
  p_main <- 
    pgrob %>% 
    gtable_filter("panel|axis")
  
  p_colorguide <- 
    pgrob %>% 
    gtable_filter("guide") %>% 
    .$grob %>% 
    .[[1]] %>% 
    gtable_filter("guide") %>% 
    .$grob %>% 
    .[[1]] %>% 
    gtable_filter("bar|label|ti") 
  
  # grid.newpage()
  # p_colorguide %>% grid.draw()
  # p_main %>% grid.draw()
  
  # define labels -------------------------------------------------
  
  label_box_y <-  
    textbox_grob(
      text = lab_y,
      height = unit(1, "npc"),
      halign = 1,
      valign = 0,
      padding = unit(
        c(0, .18, 0, 0),
        "cm"
      ),
      gp =gpar(
        fontsize = base_size,
        fontface = fontface
      ),
      box_gp = gpar(
        fill = "#FFFFFF00",
        col = "#FFFFFF00"
      )
    )  
  
  # y_and_colorguide <- 
  #   grobTree(
  #     p_colorguide,
  #     label_box_y
  #   )
  
  label_box_x <- 
    textbox_grob(
      text = lab_x,
      height = unit(1, "npc"),
      halign = 1,
      valign = 0,
      gp =gpar(
        fontsize = base_size,
        fontface = fontface
      ),
      box_gp = gpar(
        fill = "#FFFFFF00",
        col = "#FFFFFF00"
      )
    )  
  
  # put it all together -------------------------------------------
  
  p_out <- 
    p_main %>% 
    gtable_add_rows(
      heights = margin_size,
      pos = 0
    ) %>% 
    gtable_add_rows(
      heights = margin_size,
      pos = -1
    ) %>% 
    gtable_add_cols(
      widths = margin_size,
      pos = 0
    ) %>% 
    gtable_add_cols(
      widths = margin_size,
      pos = -1
    ) %>% 
    gtable_add_rows(
      height = margin_size,
      pos = 0
    ) %>% 
    gtable_add_grob(
      grobs = p_colorguide,
      t = 2,
      l = 2
    ) %>% 
    gtable_add_grob(
      grobs = label_box_y,
      t = 3,
      l = 2
    ) %>% 
    gtable_add_grob(
      grobs = label_box_x,
      t = 2,
      l = 3
    ) 
  
  p_out %>% 
    gtable_show_layout()
  
  
  # grid.newpage()
  # p_out %>% grid.draw()
  
  return(p_out)
}
