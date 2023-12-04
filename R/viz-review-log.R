build_p_review_log <- function(review_log) {
  browser()
  
  all_sections <- 
    paste("Section", rep(1:10)) %>% 
    paste(collapse = ", ") 
  
  review_log <-
    review_log %>% 
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
    # mutate(
    #   section_of_dataset = section_of_dataset %>%
    #     str_remove("Section ") %>% 
    #     as.numeric()
    #   )
    mutate(
      section_of_dataset = section_of_dataset %>% 
        as_factor()
      ) %>% 
    count(fpid, section_of_dataset)
  
  
  review_log %>% 
    ggplot() + 
    aes(x = n,
        y = fpid) +
    geom_col() +
    facet_grid(cols = vars(section_of_dataset))
}