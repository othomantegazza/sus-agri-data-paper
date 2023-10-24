library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(readxl)
library(readr)
library(janitor)
library(glue)
library(purrr)
library(jsonlite)
library(skimr)
library(here)

# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# Read selected paper data ---------------------------------------

impacts <-
  read_excel(
    'data/MerFPs_Impacts_20231017.xlsx'
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

# read column metadata ------------------------------------------

metadata <- 
  read_json(
    'data/metadata/impacts-metadata.json',
    simplifyVector = T,
  ) %>%
  as_tibble() %>% 
  rename(line_type =info_type)


# clean all data and check types---------------------------------

clean_impacts <- function(
    impacts,
    name,
    line_type,
    unique_identifiers,
    columns
) {
  
  impacts <- 
    impacts %>%
    filter(
      info_type == line_type
    ) %>% 
    select(
      all_of(
        columns$colname
      )
    ) %>% 
    distinct()
  
  impacts %>% 
    extract_duplicated_rows(
      id_cols = unique_identifiers
    ) %>% 
    write_csv(
      here(
        'data',
        'output',
        paste0(name, '-DUPL', '.csv')
      )
    )
  
  types_check <- 
    impacts %>% {
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
  
  impacts_clean <- 
    impacts
  
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
        
        impacts_clean[, i] <- 
          impacts_clean[, i,  drop = T] %>% 
          as("numeric") %>%
          as(target_type)
        
      } else {
        
        impacts_clean[, i] <- 
          impacts_clean[, i,  drop = T] %>% 
          as(target_type)
        
      }
    }
  }
  
  impacts_clean %>% 
    filter_all(
      any_vars(
        is.na(.)
      )
    ) %>% 
    write_csv(
      here(
        'data',
        'output',
        paste0(name, '-MISSING-DATA', '.csv')
      )
    )
  
  impacts_clean %>% 
    write_csv(
      here(
        'data',
        'output',
        paste0(name, '.csv')
      )
    )
  
  return(NULL)
}

metadata %>% 
  mutate(
    impacts = impacts %>% 
      list()
  ) %>% 
  pwalk(
    clean_impacts
  )
