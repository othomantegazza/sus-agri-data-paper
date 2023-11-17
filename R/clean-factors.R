clean_extract_factors <- function(
    df = 'data/MerFPs_Impacts_20231017.xlsx'
) {
  
  # Read selected paper data ---------------------------------------
  
  impacts <-
    read_excel(
      df
    )

  impacts %>%
    slice(
      impacts %>%
        problems() %>%
        pull(row)
    )

  impacts <-
    impacts %>%
    clean_names()
  
  
  # organize factors ----------------------------------------------

  impacts <- 
    impacts %>% 
    distinct(doi,
             fpid,
             impact_matrix,
             .keep_all = T) %>% 
    select(
      info_type,
      doi,
      fpid,
      impact_matrix,
      matches("factor._title"),
      matches("factor._direction")
    ) %>% 
    pivot_longer(
      cols = matches("factor")
    ) %>% 
    separate(name, into = c("factor_nb", "type"), sep = "_") %>% 
    mutate(
      factor_nb = factor_nb %>% 
        str_remove("factor") %>% 
        as.numeric()
    ) %>% 
    drop_na() %>% 
    pivot_wider(
      id_cols = c(info_type,
                  doi,
                  fpid,
                  impact_matrix,
                  factor_nb),
      names_from = type,
      values_from = value
    ) %>% 
    rename(
      factor_id = title,
      factor_direction = direction
    ) %>% 
    drop_na()
    
  
 # read column metadata ------------------------------------------

  metadata <-
    read_json(
      'data/metadata/impacts-metadata.json',
      simplifyVector = T,
    ) %>%
    as_tibble() %>%
    rename(line_type =info_type)



  # clean all data and check types---------------------------------

  metadata %>%
    filter(
      name %in% c(
        "09_factors"
      )
    ) %>%
    mutate(
      df = impacts %>%
        list()
    ) %>%
    pwalk(
      clean_imap
    )
}