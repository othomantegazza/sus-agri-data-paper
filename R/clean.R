extract_duplicated_rows <- function(df, id_cols = "doi") {
  
  stopifnot(
    all(
      id_cols %in% colnames(df)
    )
  )
  
  df <- 
    df %>% 
    distinct()
  
  df_dupl <-
    df %>% 
    count(across(all_of(id_cols))) %>% 
    filter(n > 1) %>% 
    select(-n)

  df <- 
    df_dupl %>% 
    left_join(df, by = id_cols) %>% 
    arrange(across(all_of(id_cols)))
  
  return(df)
}

count_dois <- function(impacts, message = "") {
  cat(
    "detected",
    impacts %>% pull(doi) %>% unique() %>% length(),
    "unique dois",
    message,
    ".\n"
  )
}

clean_imap <- function(
    df,
    name,
    line_type,
    unique_identifiers,
    columns
) {
  if(length(line_type) > 0) {
    df <- 
      df %>%
      filter(
        info_type %in% line_type
      )
  }
  
  df <-
    df %>% 
    select(
      all_of(
        columns$colname
      )
    ) %>% 
    distinct()
  
  df %>% 
    extract_duplicated_rows(
      id_cols = unique_identifiers
    ) %>% 
    write_excel_csv2(
      here(
        'data',
        'output',
        'diagnostics',
        paste0(name, '-DUPL', '.csv')
      ),
      escape = "double"
    )
  
  # check expected types and perform basic conversions ------------
  
  types_check <- 
    df %>% {
      tibble(
        colname = colnames(.),
        type_detected = map_chr(., class)
      )
    } %>% 
    full_join(
      columns,
      by = 'colname'
    ) %>% 
    mutate(
      types_match = 
        type_detected == type
    ) %>% 
    filter(
      ! types_match 
    )
  
  df_clean <- 
    df
  
  if(nrow(types_check) > 0) {
    warning(
      'mismatched types in ',
      name,
      ':\n- ',
      types_check %>% 
        pull(colname) %>% 
        paste(., collapse = '\n- ')
    )
    
    for(i in types_check$colname) {
      
      target_type <- 
        types_check %>% 
        filter(colname == i) %>% 
        pull(type)
      
      if(
        target_type == 'logical'
      ) {
        
        df_clean[, i] <- 
          df_clean[, i,  drop = T] %>% 
          as("numeric") %>%
          as(target_type)
        
      } else {
        
        df_clean[, i] <- 
          df_clean[, i,  drop = T] %>% 
          as(target_type)
        
      }
    }
  }
  
  df_clean %>% 
    filter_all(
      any_vars(
        is.na(.)
      )
    ) %>% 
    write_excel_csv2(
      here(
        'data',
        'output',
        'diagnostics',
        paste0(name, '-MISSING-DATA', '.csv')
      ),
      escape = "double"
    )
  
 # write clean dataset -------------------------------------------
  
  df_clean %>% 
    distinct(
      pick(all_of(unique_identifiers)),
      .keep_all = T
      ) %>% 
    mutate(
      across(
          .cols = is.character,
          .fns = ~str_replace_all(., "\r\n", " ") %>% 
            stringi::stri_trans_general("latin-ascii") %>% 
            str_replace_all(";", ",")
      )
    ) %>% 
    write_excel_csv2(
      here(
        'data',
        'output',
        'imap',
        paste0(name, '.csv')
      ),
      escape = "double"
    )
  
  return(NULL)
}
