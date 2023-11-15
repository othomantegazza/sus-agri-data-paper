clean_identifiers <- function(df = "data/MerFPs_DefinitionFP_20231017.xlsx") {
  # data ----------------------------------------------------------
  
  defs <- 
    read_excel(df) %>% 
    clean_names() %>% 
    drop_na(text) %>% 
    rename(type = x1)
  
  # metadata ------------------------------------------------------
  
  metadata <- 
    read_json(
      'data/metadata/impacts-metadata.json',
      simplifyVector = T,
    ) %>%
    as_tibble() %>% 
    rename(line_type = info_type) %>% 
    filter(name == "01_fp_definitions")
  
  # clean ---------------------------------------------------------
  
  metadata %>% 
    mutate(df = defs %>% list()) %>% 
    pwalk(
      clean_imap
    )
}

