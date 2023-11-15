clean_screening <- function(
    df = 'data/MerFPs_Screening_20231017.xlsx'
) {
  # read data -----------------------------------------------------
  
  screening <- 
    read_excel(
      df
    ) %>% 
    clean_names()
  
  
  # metadata ------------------------------------------------------
  
  metadata <- 
    read_json(
      'data/metadata/impacts-metadata.json',
      simplifyVector = T,
    ) %>%
    as_tibble() %>% 
    rename(line_type = info_type) %>% 
    filter(name == "04_screeening")
  
  
  screening_status <-
    read_json(
      'data/metadata/screening-status.json',
      simplifyVector = T
    ) %>% 
    {set_names(
      x = unlist(.),
      nm = names(.)
    )}
  
  # extract screening statuses ------------------------------------
  
  systematic_screening <-
    screening %>%
    mutate(status = status %>% toupper()) %>% 
    mutate(status = status %>% {
      case_when(
        . == "LB" ~ "O",
        . == "LB NOT IN EU" ~ "O",
        . == "Y" ~ "O",
        . == "G" ~ "R",
        TRUE ~ .
      )
    }) %>% 
    mutate(year = year %>% as.numeric())
  
  metadata %>% 
    mutate(df = systematic_screening %>% list()) %>% 
    pwalk(
      clean_imap
    )
}