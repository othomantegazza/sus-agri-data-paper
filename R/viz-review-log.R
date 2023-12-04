build_p_review_log <- function(
    review_log, 
    search_tab
    ) 
{
  browser()
  
  all_sections <- 
    paste("Section", rep(1:10)) %>% 
    paste(collapse = ", ") 
  
  review_log <-
    review_log %>% 
    mutate(
      fpid = fpid %>% 
        {
          case_when(
            . == "Agroforestry" ~ "Agroforestry systems",
            . == "All FPs" ~ "All",
            . == "Conversion of forests into agroforestry" ~ "Agroforestry systems",
            . == "Cover crop" ~ "Cover and catch crops",
            . == "Enhanced-efficiency fertilisers" ~ "Enhanced efficiency fertilisers",
            . == "Grazing" ~ "Grazing management",
            . == "Green Manure" ~ "Green manure",
            . == "Livestock feeding techniques" ~ "Livestock dietary manipulation techniques",
            . == "Livestock housin" ~ "Livestock housing techniques",
            . == "Low ammonia" ~ "Low ammonia techniques for mineral fertilisation",
            . == "Low ammonia emission techiniques" ~ "Low ammonia techniques for mineral fertilisation",
            . == "Low ammonia techniques for mineral fertilisation" ~ "Low ammonia techniques for mineral fertilisation",
            . == "Manure land application" ~ "Manure land application techniques",
            . == "Livestock housing" ~ "Livestock housing techniques",
            . == "No tillage" ~ "No tillage and reduced tillage",
            . == "No-irrigation" ~ "No irrigation",
            . == "Pesticide reduction strategies" ~ "Pesticides reduction strategies",
            . == "Sustainable water management in flooded land" ~ "Water-saving irrigation practices in flooded lands",
            . == "Sustainable water management in flooded lands" ~ "Water-saving irrigation practices in flooded lands",
            . == "Water saving techniques in flooded land" ~ "Water-saving irrigation practices in flooded lands",
            . == "Sustainable water management in non-flooded land" ~ "Water-saving irrigation practices in non-flooded lands",
            . == "Sustainable water management in non-flooded lands" ~ "Water-saving irrigation practices in non-flooded lands",
            . == "Sustainable water management in non-flooded lands_spreadsheet_240723" ~ "Water-saving irrigation practices in non-flooded lands",
            . == "Water saving techniques in non-flooded land" ~ "Water-saving irrigation practices in non-flooded lands",
            TRUE ~ .
          )
        }
    ) %>% 
    mutate(
      section_of_dataset = section_of_dataset %>% 
        {
          case_when(. == "All sections" ~ all_sections,
                    TRUE ~ .)
        }
    ) %>% 
    mutate(
      section_of_dataset = section_of_dataset %>% 
        str_split(", ")
    ) %>% 
    unnest(section_of_dataset) %>% 
    mutate(
      section_of_dataset = section_of_dataset %>% 
        as_factor()
      ) %>% 
    count(fpid, section_of_dataset)
  
  
  review_log %>% 
    ggplot() + 
    aes(x = section_of_dataset,
        y = fpid,
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
}
