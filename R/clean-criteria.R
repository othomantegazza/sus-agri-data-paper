clean_criteria <- function(
    df = 'data/MerFPs_Criteria_20231017.xlsx'
) {
  # read data -----------------------------------------------------
  
  criteria <- 
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
    filter(name == "03_selection_criteria")
  
  # clean ---------------------------------------------------------
  
  criteria <- 
    criteria %>% 
    select(
      fpid,
      criteria_for_exclusion,
      criteria_for_inclusion
    ) %>% 
    pivot_longer(-fpid,
                 names_to = "used_for",
                 values_to = "criteria") %>% 
    drop_na(criteria) %>% 
    mutate(used_for = used_for %>% str_remove("criteria_for_")) %>% 
    arrange(fpid, used_for)
  
  metadata %>% 
    mutate(df = criteria %>% list()) %>% 
    pwalk(
      clean_imap
    )
}